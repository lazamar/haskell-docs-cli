{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module HoogleCli
  ( interactive
  , evaluate
  , ShellState(..)
  , Context(..)
  , Cmd(..)
  , Selection(..)
  , packageUrl
  , runCLI
  ) where

import Prelude hiding (mod)
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad.Trans.Class (lift)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT(..), MonadError, catchError, runExceptT, throwError)
import Control.Monad.Catch (MonadThrow)
import Control.Concurrent.MVar (MVar)
import Control.Monad.State.Lazy (MonadState, StateT)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.List hiding (groupBy)
import Data.List.Extra (breakOn)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import System.Environment (getEnv)
import System.IO (hClose, stdout, Handle)
import System.IO.Temp (withSystemTempFile)

import HoogleCli.Types
import HoogleCli.Haddock

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad.State.Lazy as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text (hPutStr)
import qualified Hoogle
import qualified Network.HTTP.Client as Http
import qualified System.Console.Haskeline as CLI
import qualified System.Process as Process
import qualified System.Console.Terminal.Size as Terminal
import qualified Text.PrettyPrint.ANSI.Leijen as P


data ShellState = ShellState
  { sContext :: Context
  , sManager :: Http.Manager
  , sCache :: Map Url (MVar ByteString)
  }

-- | Context referenced by commands that contain an index
data Context
  = ContextEmpty                        -- ^ Nothing selected
  | ContextSearch String [TargetGroup]  -- ^ within search results
  | ContextModule Module                -- ^ looking at module docs
  | ContextPackage Package              -- ^ looking at a a package's modules

type Index = Int

-- | Commands we accept
data Cmd
  = ViewAny View Selection           -- ^ by default we do a Hoogle search or
                                     -- view/index the current context
  | ViewSource Selection             -- ^ source for target
  | ViewModule View Selection
  | ViewExtendedDocs Index           -- ^ declaration's docs available in the haddock page
  | ViewModuleDocs Selection         -- ^ full haddock for module
  | ViewModuleInterface Selection    -- ^ all function signatures
  | ViewPackageModules Selection     -- ^ list modules from a package
  | Quit

-- data Cmd
--   = ViewAny View Selection
--   | ViewDeclaration View Selection
--   | ViewPackage View Selection
--   | ViewModule View Selection
--   | Quit

data Selection
  = Search String
  | ItemIndex Index
  | SelectContext

data View =  Interface | Documentation

newtype M a = M { runM :: ExceptT String (StateT ShellState (CLI.InputT IO)) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState ShellState
    , MonadError String
    , MonadIO
    , MonadThrow
    , MonadFail
    )

runCLI :: CLI.Settings IO -> ShellState -> M a -> IO (Either String a)
runCLI settings state
  = CLI.runInputT settings
  . flip State.evalStateT state
  . runExceptT
  . runM

class MonadCLI m where
  getInputLine :: String -> m (Maybe String)

instance MonadCLI M where
  getInputLine str = M $ lift $ lift $ CLI.getInputLine str

runSearch :: String -> M [Hoogle.Target]
runSearch term = do
  req <- Http.parseRequest "https://hoogle.haskell.org"
    <&> Http.setQueryString
      [ ("mode", Just "json")
      , ("start", Just "1")
      , ("hoogle", Just $ Text.encodeUtf8 $ Text.pack term)
      ]
  res <- fetch req
  either error return $ Aeson.eitherDecode res

withFirstSearchResult
  :: String
  -> (Hoogle.Target -> Bool)
  -> String
  -> (Hoogle.Target -> M a)
  -> M a
withFirstSearchResult name valid term act = do
  res <- toGroups <$> runSearch term
  firstResult <- case NonEmpty.head <$> listToMaybe res of
    Nothing -> throwError $ "No results found for '" <> term <> "'"
    Just x -> return x
  State.modify' (\s -> s{ sContext = ContextSearch term res })
  if valid firstResult
    then act firstResult
    else do
      viewSearchResults res
      throwError $ "First search result is not a " <> name

hooglePackageUrl :: String -> PackageUrl
hooglePackageUrl pname =
  PackageUrl $ "https://hackage.haskell.org/package/" ++ pname

toGroups :: [Hoogle.Target] -> [TargetGroup]
toGroups
  = mapMaybe NonEmpty.nonEmpty
  . groupBy relevantFields
  where
    relevantFields target = target
      { Hoogle.targetURL = ""
      , Hoogle.targetPackage = Nothing
      , Hoogle.targetModule = Nothing
      }

groupBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupBy f vs = go mempty vs
  where
    go res []
      = map reverse
      $ reverse
      $ fst
      $ foldr takeOnce ([], res)
      $ reverse vs
    go res (x:xs) = go newRes xs
      where newRes = Map.insertWith (++) (f x) [x] res

    takeOnce x (out, m) =
      let key = f x in
      case Map.lookup key m of
        Nothing -> (out, m)
        Just v -> (v:out, Map.delete key m)

commands :: [String]
commands =
  [ "documentation"
  , "src"
  , "quit"
  , "module-doc"
  , "edoc"
  , "interface"
  , "package"
  ]

fillPrefix :: String -> Maybe String
fillPrefix v = find (v `isPrefixOf`) commands

parseCommand :: String -> Either String Cmd
parseCommand str = case str of
  (':':xs) -> do
    let (mcmd, args) = bimap fillPrefix (drop 1) $ break isSpace xs
    cmd <- maybe (Left "Unknown command") Right mcmd
    let intCmd f
          | Just n <- readMaybe args = Right (f n)
          | otherwise = Left $
            "Command :" <> cmd <> "expects an integer argument"

        selection
          | null args                = SelectContext
          | Just n <- readMaybe args = ItemIndex n
          | otherwise                = Search args

    case cmd of
        -- any
        "documentation" -> Right $ ViewAny Documentation selection
        "interface"     -> Right $ ViewAny Interface selection
        "src"           -> Right $ ViewSource selection

        -- module
        "module"         -> Right $ ViewModule Interface selection
        "mdocumentation" -> Right $ ViewModule Documentation selection
        "minterface"     -> Right $ ViewModule Interface selection

        "module-doc"    -> Right $ ViewModuleDocs selection
        "edoc"          -> intCmd ViewExtendedDocs
        "package"       -> Right $ ViewPackageModules selection
        "quit"          -> Right Quit
        _ -> error $ "Unknown command: " <> cmd
  -- no colon cases
  x | Just n <- readMaybe x -> Right $ ViewAny Interface $ ItemIndex n
  []                        -> Right $ ViewAny Interface SelectContext
  _                         -> Right $ ViewAny Interface $ Search str

interactive :: M ()
interactive = do
  context <- State.gets sContext
  case context of
    ContextEmpty      -> return ()
    ContextSearch t _ -> liftIO $ putStrLn $ "search: " <> t
    ContextModule m   -> liftIO $ putStrLn $ "module: " <> mTitle m
    ContextPackage p  -> liftIO $ putStrLn $ "package: " <> pTitle p
  minput <- getInputLine "hoogle> "
  case parseCommand $ fromMaybe "" minput of
    Left err   -> liftIO (putStrLn err) >> interactive
    Right Quit -> return ()
    Right cmd  -> do
      let showFailure e = liftIO $ putStrLn $ "Failed: "<> e
      evaluate cmd `catchError` showFailure
      interactive

evaluate :: Cmd -> M ()
evaluate cmd = State.gets sContext >>= \context -> case cmd of
  Quit -> error "impossible"

  -- pressed enter without typing anything
  ViewAny Interface SelectContext ->
    case context of
      ContextEmpty            -> return ()
      ContextSearch _ results -> viewSearchResults results
      ContextModule mdocs     -> viewModuleInterface mdocs
      ContextPackage package  -> viewPackageInterface package

  -- <TERM>
  ViewAny Interface (Search term) -> do
    res <- toGroups <$> runSearch term
    viewSearchResults res
    State.modify' $ \s -> s{ sContext = ContextSearch term res }

  ViewAny Interface (ItemIndex ix) ->
    case context of
      ContextEmpty -> errEmptyContext
      ContextSearch _ results ->
        withTargetGroup ix results $ viewInTerminalPaged . viewFull
      ContextModule mod -> do
        decl <- elemAt ix (mDeclarations mod)
        viewInTerminalPaged $ P.vsep
          [ prettyDecl decl
            -- ad-hoc link colour
          , P.cyan $ P.text $ getUrl (dModuleUrl decl)
          ]
      ContextPackage package ->
        withModuleFromPackage ix package viewModuleInterface

  -- :documentation
  ViewAny Documentation SelectContext ->
    case context of
      ContextEmpty            -> errEmptyContext
      ContextSearch _ results -> viewSearchResults results
      ContextModule mod       -> viewModuleDocs mod
      ContextPackage package  -> viewPackageDocs package

  -- :documentation <TERM>
  ViewAny Documentation (Search term) ->
    withFirstSearchResult "module" isModule term
      $ flip withModuleForTarget viewModuleDocs

  -- :documentation <INDEX>
  ViewAny Documentation (ItemIndex ix) ->
    case context of
      ContextEmpty            -> errEmptyContext
      ContextSearch _ results ->
        withTargetGroup ix results $ \tgroup -> do
        let target = NonEmpty.head tgroup
        case targetType target of
          TModule      -> withModuleForTarget target viewModuleDocs
          TPackage     -> withPackageForTarget target viewPackageDocs
          TDeclaration ->
            withModuleForTarget target $ \mod ->
            viewInTerminalPaged $ case targetDeclaration target mod of
              Just decl -> prettyDecl decl
              Nothing -> viewItem target
      ContextModule mod      -> withDeclFromModule ix mod viewDeclaration
      ContextPackage package -> withModuleFromPackage ix package viewModuleDocs

  -- :src
  ViewSource SelectContext ->
    throwError "no declaration selected. Use ':src INT'"

  -- :src <TERM>
  ViewSource (Search term) ->
    withFirstSearchResult "declaration" isDecl term
        (maybe errNoSourceAvailable viewSource . targetDeclUrl)

  -- :src <INDEX>
  ViewSource (ItemIndex ix) ->
    case context of
      ContextEmpty            -> errEmptyContext
      ContextSearch _ results ->
        withTargetGroup ix results
          (maybe errNoSourceAvailable viewSource . targetDeclUrl . NonEmpty.head)
      ContextModule mod       ->
        withDeclFromModule ix mod (viewSource . declUrl)
      ContextPackage _        -> errSourceOnlyForDeclarations

  -- :minterface
  ViewModule Interface SelectContext ->
    case context of
      ContextModule mod -> viewModuleInterface mod
      _                 -> throwError "not in a module context"

  -- :minterface <TERM>
  ViewModule Interface (Search term) ->
    withFirstSearchResult "module" isModule term $ \target ->
    withModuleForTarget target $ \mod ->
    viewModuleInterface mod

  -- :minterface <INDEX>
  ViewModule Interface (ItemIndex ix) ->
    case context of
      ContextEmpty            -> errEmptyContext
      ContextSearch _ results ->
        withTarget ix results $ \target ->
        withModuleForTarget target $ \mod ->
        viewModuleInterface mod
      ContextModule mod       -> viewModuleInterface mod
      ContextPackage package  ->
        withModuleFromPackage ix package viewModuleInterface

  ViewModule Documentation (Search term) ->
    case context of
      ContextModule mod -> viewModuleInterface mod
      _                 -> throwError "not in a module context"

  ViewModule Documentation _ -> errSourceOnlyForDeclarations

  ViewExtendedDocs ix ->
    getTargetGroup' ix $ \tgroup -> do
    let target = NonEmpty.head tgroup
        url = moduleUrl target
    html <- fetch' url
    let modl = parseModuleDocs url html
        desc = case targetDeclUrl target of
          Nothing -> prettyHtml <$> mDescription modl
          Just (DeclUrl _ anchor) -> prettyDecl <$> lookupDecl anchor modl
    viewInTerminalPaged $ fromMaybe mempty desc

  ViewModuleInterface selection ->
    case selection of
      SelectContext ->
        case context of
          ContextModule m -> viewModuleInterface m
          _ -> throwError "no module specified"
      Search term ->
        withFirstSearchResult "module" isModule term $ \target ->
          withModuleForTarget target viewModuleInterface
      ItemIndex ix ->
        getTarget' ix $ \target ->
          withModuleForTarget target viewModuleInterface

  ViewModuleDocs selection ->
    case selection of
      SelectContext -> withModuleContext viewModuleDocs
      Search term -> withFirstSearchResult "module" isModule term viewTargetModuleDocs
      ItemIndex ix -> getTarget' ix viewTargetModuleDocs
    where
      viewTargetModuleDocs target = do
        let url = moduleUrl target
        html <- fetch' url
        viewModuleDocs (parseModuleDocs url html)

  ViewPackageModules (Search term) -> do
    let url = hooglePackageUrl term
    html <- fetch' url
    let package = parsePackageDocs url html
    viewPackageInterface package
    State.modify' $ \s -> s{ sContext = ContextPackage package }

  ViewPackageModules (ItemIndex ix) ->
    case context of
      ContextSearch _ _ ->
        getTarget' ix $ \target -> do
        let url = packageUrl $ moduleUrl target
        html <- fetch' url
        let package = parsePackageDocs url html
        viewPackageInterface package
        State.modify' $ \s -> s{ sContext = ContextPackage package }
      _ -> throwError $ "no package for index " <> show ix

  ViewPackageModules SelectContext -> do
    package <- case context of
      ContextEmpty -> throwError "no package to show"
      ContextSearch _ _ -> throwError "no index selected"
      ContextModule module' -> do
        let url = packageUrl $ mUrl module'
        html <- fetch' url
        let package = parsePackageDocs url html
        viewPackageInterface package
        return package
      ContextPackage package -> do
        viewPackageInterface package
        return package
    State.modify' $ \s -> s{ sContext = ContextPackage package }

-- errors
errSourceOnlyForDeclarations :: M a
errSourceOnlyForDeclarations =
    throwError "can only view source of declarations"

errEmptyContext :: M a
errEmptyContext =
  throwError "empty context"

errNoSourceAvailable :: M a
errNoSourceAvailable =
 throwError "no source available for that declaration"

targetDeclaration :: Hoogle.Target -> Module -> Maybe Declaration
targetDeclaration target mod = do
  DeclUrl _ anchor <- targetDeclUrl target
  lookupDecl anchor mod

-- TODO: handle if target is a package
withModuleForTarget
  :: Hoogle.Target
  -> (Module -> M ())
  -> M ()
withModuleForTarget target act = do
  let url = moduleUrl target
  html <- fetch' url
  let mod = parseModuleDocs url html
  State.modify' $ \s -> s{ sContext = ContextModule mod }
  act mod

-- TODO
withPackageForTarget :: Hoogle.Target -> (Package -> M ()) -> M ()
withPackageForTarget = error "TODO"

-- | Get an element from a one-indexed index
elemAt :: Int -> [a] -> M a
elemAt ix =
  maybe (throwError "index out of range") return
  . listToMaybe
  . drop (ix - 1)

withSearchContext :: ([TargetGroup] -> M a) -> M a
withSearchContext f = do
  context <- State.gets sContext
  case context of
    ContextSearch _ results -> f results
    _ -> throwError "no search results available"

withModuleContext :: (Module -> M a) -> M a
withModuleContext f = do
  context <- State.gets sContext
  case context of
    ContextModule mdocs -> f mdocs
    _ -> throwError "No module selected"

withTarget :: Int -> [TargetGroup] -> (Hoogle.Target -> M a) -> M a
withTarget ix results act = do
  tgroup <- elemAt ix results
  target <- promptSelectOne tgroup
  act target

withTargetGroup :: Int -> [TargetGroup] -> (TargetGroup -> M a) -> M a
withTargetGroup ix groups act = do
  tgroup <- elemAt ix groups
  act tgroup

withModuleFromPackage :: Int -> Package -> (Module -> M a) -> M a
withModuleFromPackage ix (Package _ modules purl) act = do
  url <- packageModuleUrl purl <$> elemAt ix modules
  html <- fetch' url
  let mod = parseModuleDocs url html
  State.modify' $ \s -> s{ sContext = ContextModule mod }
  act mod

withDeclFromModule :: Int -> Module -> (Declaration -> M a) -> M a
withDeclFromModule ix mod act = do
  decl <- elemAt ix (mDeclarations mod)
  act decl

-- TODO: deprecate
getTarget' :: Int -> (Hoogle.Target -> M a) -> M a
getTarget' ix f = getTargetGroup' ix (f <=< promptSelectOne)

-- TODO: deprecate
getTargetGroup' :: Int -> (TargetGroup -> M a) -> M a
getTargetGroup' ix f = withSearchContext $ \results -> do
  el <- elemAt ix results
  f el

viewSearchResults :: MonadIO m => [TargetGroup] -> m ()
viewSearchResults
  = viewInTerminal
  . P.vsep
  . reverse
  . numbered
  . map viewSummary

viewDeclaration :: MonadIO m => Declaration -> m ()
viewDeclaration = viewInTerminalPaged . prettyDecl

viewModuleInterface :: MonadIO m => Module -> m ()
viewModuleInterface
  = viewInTerminalPaged
  . P.vsep
  . numbered
  . map (prettyHtml . dSignature)
  . mDeclarations

viewModuleDocs :: MonadIO m => Module -> m ()
viewModuleDocs = viewInTerminalPaged . prettyModule

viewPackageInterface :: MonadIO m => Package -> m ()
viewPackageInterface (Package _ modules _) =
  viewInTerminalPaged $ P.vsep $ numbered (P.text <$> modules)

viewPackageDocs :: MonadIO m => Package -> m ()
viewPackageDocs = error "TODO"

promptSelectOne :: TargetGroup -> M Hoogle.Target
promptSelectOne tgroup =
  case toList tgroup of
    [target] -> return target
    targets -> do
      liftIO $ do
        putStrLn "Select one:"
        viewInTerminal
          $ P.vsep
          $ numbered
          $ mapMaybe viewPackageAndModule targets

      num <- getInputLine ": "
      case readMaybe =<< num of
        Just n -> case listToMaybe $ drop (n - 1) targets of
          Just target -> return target
          Nothing -> do
            liftIO $ putStrLn "Invalid index"
            promptSelectOne tgroup
        Nothing -> do
          liftIO $ putStrLn "Number not recognised"
          promptSelectOne tgroup

viewInTerminal :: MonadIO m => P.Doc -> m ()
viewInTerminal = printDoc stdout

viewInTerminalPaged :: MonadIO m => P.Doc -> m ()
viewInTerminalPaged doc = withPager $ \handle -> printDoc handle doc

withPager :: MonadIO m => (Handle -> IO a)  -> m a
withPager act = liftIO $ do
  let cmd = (Process.proc "less" ["-FRX"]) { Process.std_in = Process.CreatePipe }
  Process.withCreateProcess cmd
     $ \(Just hin) _ _ p -> do
       res <- act hin `finally` hClose hin
       _   <- Process.waitForProcess p
       return res

-- | Maximum screen width for flowing text.
-- Fixed-width portions will still overflow that.
maxWidth :: Int
maxWidth = 80

printDoc :: MonadIO m => Handle -> P.Doc -> m ()
printDoc handle doc = liftIO $ do
  width <- min maxWidth . maybe maxWidth Terminal.width <$> Terminal.size
  P.displayIO handle $ P.renderSmart 1 width doc
  putStrLn ""

viewSource :: DeclUrl -> M ()
viewSource durl = do
  link <- sourceLink durl
  html <- fetch' link
  viewInEditor (fileInfo link html)
  where
    viewInEditor :: FileInfo -> M ()
    viewInEditor (FileInfo filename mline content) = do
      let line = maybe "" (("+" <>) . show) mline
      liftIO $ do
        editor <- getEditor
        withSystemTempFile filename $ \fullpath handle -> do
          Text.hPutStr handle content
          Process.callCommand $ unwords [editor, fullpath, line]

getEditor :: IO String
getEditor = getEnv "VISUAL" <|> getEnv "EDITOR" <|> defaultEditor
  where
    defaultEditor = error "no editor selected"

class HasUrl a where
  getUrl :: a -> Url
instance HasUrl DeclUrl    where getUrl (DeclUrl url anchor) =
                                   getUrl url ++ "#" ++ Text.unpack anchor
instance HasUrl ModuleUrl  where getUrl (ModuleUrl url) = url
instance HasUrl SourceLink where getUrl (SourceLink url _) = url
instance HasUrl PackageUrl where getUrl (PackageUrl url) = url
instance HasUrl Url        where getUrl url = url

fetch' :: HasUrl a => a -> M HtmlPage
fetch' x = do
  req <- liftIO $ Http.parseRequest $ getUrl x
  src <- fetch req
  return (parseHtmlDocument src)

fetch :: Http.Request -> M LB.ByteString
fetch req = do
  cache <- State.gets sCache
  let key = show req
  case Map.lookup key cache of
    Just mvar -> liftIO $ MVar.readMVar mvar
    Nothing -> do
      manager <- State.gets sManager
      mvar <- liftIO MVar.newEmptyMVar
      State.modify $ \s -> s { sCache = Map.insert key mvar (sCache s) }
      liftIO $ do
        res <- Http.responseBody <$> Http.httpLbs req manager
        MVar.putMVar mvar res
        return res

isModule :: Hoogle.Target -> Bool
isModule target = TModule == targetType target

isDecl :: Hoogle.Target -> Bool
isDecl target = TDeclaration == targetType target

data TargetType = TModule | TPackage | TDeclaration
  deriving (Show, Eq)

targetType :: Hoogle.Target -> TargetType
targetType target =
  case Hoogle.targetType target of
    "module" -> TModule
    "package" -> TPackage
    _ -> TDeclaration

-- ================================
-- Pretty printing
-- ================================

-- TODO rename to view declaration and use declaration type
viewItem :: Hoogle.Target -> P.Doc
viewItem = prettyHtml . parseHoogleHtml . Hoogle.targetItem

viewSummary :: TargetGroup -> P.Doc
viewSummary tgroup = P.vsep
  [ viewItem $ NonEmpty.head tgroup
  , viewPackageInfoList tgroup
  ]

viewPackageInfoList :: TargetGroup -> P.Doc
viewPackageInfoList
  = P.group
  . P.fillSep
  . P.punctuate P.comma
  . mapMaybe viewPackageAndModule
  . toList

viewPackageAndModule :: Hoogle.Target -> Maybe P.Doc
viewPackageAndModule target = do
  pkg <- P.magenta . P.text . fst <$> Hoogle.targetModule target
  mol <- P.black . P.text . fst <$> Hoogle.targetPackage target
  return $ pkg P.<+> mol

prettyModule :: Module -> P.Doc
prettyModule (Module name minfo decls _) =
  P.vsep $ [title]
    ++ [ prettyHtml info | Just info <- [minfo] ]
    ++ [ prettyDecl decl | decl <- decls ]
  where
    title = P.vsep
      [ P.text name
      , P.text $ replicate (length name) '='
      , ""
      ]

prettyDecl :: Declaration -> P.Doc
prettyDecl Declaration{..} =
  P.vsep $ map prettyHtml (dSignature:dContent)

lookupDecl :: Anchor -> Module -> Maybe Declaration
lookupDecl anchor (Module _ _ decls _) =
  find (Set.member anchor . dAnchors) decls

viewFull :: TargetGroup -> P.Doc
viewFull tgroup = P.vsep
  [ divider
  , content
  , divider
  ]
  where
    divider = P.black $ P.text $ replicate 50 '='
    representative = NonEmpty.head tgroup
    content = P.vsep $
      [ viewItem representative
      , viewPackageInfoList tgroup
      , prettyHtml $ parseHoogleHtml $ Hoogle.targetDocs representative
      ] ++ reverse (P.cyan . P.text . Hoogle.targetURL <$> toList tgroup)

-- ================================
-- Hoogle handling
-- ================================

-- | Get URL for source file for a target
sourceLink :: DeclUrl -> M SourceLink
sourceLink (DeclUrl murl anchor) = do
  html <- fetch' murl
  let links = sourceLinks murl html
  case lookup anchor links of
    Nothing -> throwError $ unlines $
      [ "anchor missing in module docs"
      , show murl
      ] ++ map show links
    Just link -> return link

-- | Get URL for module documentation
moduleUrl :: Hoogle.Target -> ModuleUrl
moduleUrl target = ModuleUrl (dropAnchor url)
  where
    url = Hoogle.targetURL target

declUrl :: Declaration -> DeclUrl
declUrl Declaration{..} =  DeclUrl dModuleUrl dAnchor

targetDeclUrl :: Hoogle.Target -> Maybe DeclUrl
targetDeclUrl target = do
  anchor <- takeAnchor url
  return $ DeclUrl (moduleUrl target) anchor
  where
    url = Hoogle.targetURL target

packageUrl :: ModuleUrl -> PackageUrl
packageUrl (ModuleUrl url) = PackageUrl $ fst $ breakOn "docs" url

packageModuleUrl :: PackageUrl -> String -> ModuleUrl
packageModuleUrl (PackageUrl purl) moduleName =
  ModuleUrl url
  where
    url =
      stripSuffix "/" purl
      ++ "/docs/"
      ++ map (replace '.' '-') moduleName
      ++ ".html"
    -- replace this with that
    replace this that x
      | x == this = that
      | otherwise = x

    stripSuffix x s = maybe s reverse $ stripPrefix x $ reverse s

