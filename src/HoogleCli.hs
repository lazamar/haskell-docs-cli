{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wwarn #-}
module HoogleCli
  ( interactive
  , evaluate
  , evaluateCmd
  , ShellState(..)
  , Context(..)
  , Cmd(..)
  , Selection(..)
  , View(..)
  , packageUrl
  , runCLI
  ) where

import Prelude hiding (mod)
import Control.Applicative ((<|>))
import Control.Exception (finally, throwIO)
import Control.Monad (unless, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT(..), MonadError, catchError, runExceptT, throwError)
import Control.Monad.Catch (MonadThrow)
import Control.Concurrent.MVar (MVar)
import Control.Monad.State.Lazy (MonadState, StateT(..))
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes)
import Data.List hiding (groupBy)
import Data.List.Extra (breakOn)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, stdout, Handle)
import System.IO.Temp (withSystemTempFile)
import System.Exit (exitSuccess)
import qualified Hoogle as H

import HoogleCli.Types
import HoogleCli.Haddock as Haddock
import qualified HoogleCli.Hoogle as Hoogle

import qualified Control.Concurrent.Async as Async
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
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http
import qualified System.Console.Haskeline as CLI
import qualified System.Process as Process
import qualified System.Console.Terminal.Size as Terminal
import qualified Text.PrettyPrint.ANSI.Leijen as P


data ShellState = ShellState
  { sContext :: Context
  , sManager :: Http.Manager
  , sCache :: Map Url (MVar ByteString)
  }

type TargetGroup = NonEmpty Hoogle.Item

-- | Context referenced by commands that contain an index
data Context
  = ContextEmpty                        -- ^ Nothing selected
  | ContextSearch String [TargetGroup]  -- ^ within search results
  | ContextModule Haddock.Module        -- ^ looking at module docs
  | ContextPackage Haddock.Package      -- ^ looking at a a package's modules

type Index = Int

-- | Commands we accept
data Cmd
  = ViewAny View Selection
  -- ^ by default we do a Hoogle search or view/index the current context
  | ViewDeclarationSource Selection
  | ViewDeclaration Selection
  | ViewModule View Selection
  | ViewPackage View Selection
  | Quit

data Selection
  = SelectContext
  | SelectByIndex Index
  | SelectByPrefix String
  | Search String

data View =  Interface | Documentation

newtype M a = M { runM :: ExceptT String (CLI.InputT (StateT ShellState IO)) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError String
    , MonadIO
    , MonadThrow
    , MonadFail
    )

instance MonadState ShellState M where
  state f = M $ lift $ lift $ State.state f

runCLI :: ShellState -> M a -> IO (Either String a)
runCLI state
  = flip State.evalStateT state
  . CLI.runInputT cliSettings
  . runExceptT
  . runM

cliSettings :: CLI.Settings (StateT ShellState IO)
cliSettings = (CLI.defaultSettings :: CLI.Settings (StateT ShellState IO))
  { CLI.complete = complete }

complete :: CLI.CompletionFunc (StateT ShellState IO)
complete (left', _) = do
  let left = reverse left'
  case left of
    ':':xs | not (any isSpace xs) , Just cmd <- fillPrefix xs ->
      return (":", [CLI.simpleCompletion cmd])
    '/':xs -> do
        context <- State.gets sContext
        let prefixes = case context of
              ContextEmpty -> []
              ContextSearch _ tgroups -> map (completion . NonEmpty.head) tgroups
              ContextModule m -> map completion (mDeclarations m)
              ContextPackage p -> pModules p
        return
          ( left'
          , [ CLI.Completion
              { CLI.replacement = drop (length xs) option
              , CLI.display = option
              , CLI.isFinished = True
              }
            | option <- prefixes
            , xs `isPrefixOf` option
            ]
          )
    _ -> return (left', [])



class MonadCLI m where
  getInputLine :: String -> m (Maybe String)

instance MonadCLI M where
  getInputLine str = M $ lift $ CLI.getInputLine str

runSearch :: String -> M [Hoogle.Item]
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
  :: (String, Hoogle.Item -> Maybe x)
  -> String
  -> (x -> M a)
  -> M a
withFirstSearchResult (name, get) term act = do
  allResults <- runSearch term
  let res = toGroups allResults
  State.modify' (\s -> s{ sContext = ContextSearch term res })
  case listToMaybe (mapMaybe get allResults) of
    Just firstValid ->
      act firstValid
    Nothing -> do
      viewSearchResults res
      throwError $ "No " <> name <> " results found for '" <> term <> "'"

hooglePackageUrl :: String -> PackageUrl
hooglePackageUrl pname =
  PackageUrl $ "https://hackage.haskell.org/package/" ++ pname

toGroups :: [Hoogle.Item] -> [TargetGroup]
toGroups
  = mapMaybe NonEmpty.nonEmpty
  . groupBy relevantFields
  where
    relevantFields item = target
      { H.targetURL = ""
      , H.targetPackage = Nothing
      , H.targetModule = Nothing
      }
      where
        target = case item of
            Hoogle.Declaration x -> Hoogle.dTarget x
            Hoogle.Module x      -> Hoogle.mTarget x
            Hoogle.Package x     -> Hoogle.pTarget x

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
  , "interface"
  , "src"

  , "declaration"

  , "module"
  , "mdocumentation"
  , "minterface"

  , "package"
  , "pdocumentation"
  , "pinterface"

  , "quit"
  ]

fillPrefix :: String -> Maybe String
fillPrefix v = find (v `isPrefixOf`) commands

parseCommand :: String -> Either String Cmd
parseCommand str = case str of
  (':':xs) -> do
    let (mcmd, args) = bimap fillPrefix (drop 1) $ break isSpace xs
    cmd <- maybe (Left "Unknown command") Right mcmd
    let selection
          | null args                = SelectContext
          | ('/':prefix) <- args     = SelectByPrefix prefix
          | Just n <- readMaybe args = SelectByIndex n
          | otherwise                = Search args

    Right $ case cmd of
      -- any
      "documentation"  -> ViewAny Documentation selection
      "interface"      -> ViewAny Interface selection
      "src"            -> ViewDeclarationSource selection
      -- declaration
      "declaration"    -> ViewDeclaration selection
      -- module
      "module"         -> ViewModule Interface selection
      "mdocumentation" -> ViewModule Documentation selection
      "minterface"     -> ViewModule Interface selection
      -- package
      "package"        -> ViewPackage Interface selection
      "pdocumentation" -> ViewPackage Documentation selection
      "pinterface"     -> ViewPackage Interface selection
      "quit"           -> Quit
      _ -> error $ "Unknown command: " <> cmd
  -- no colon cases
  x | Just n <- readMaybe x -> Right $ ViewAny Interface $ SelectByIndex n
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
  evaluate $ fromMaybe "" minput
  interactive

evaluate :: String -> M ()
evaluate input =
  case parseCommand input of
    Left err   -> liftIO (putStrLn err)
    Right cmd  -> evaluateCmd cmd `catchError` showFailure
  where
    showFailure e = liftIO $ putStrLn $ "Failed: "<> e

evaluateCmd :: Cmd -> M ()
evaluateCmd cmd = State.gets sContext >>= \context -> case cmd of
  Quit -> liftIO exitSuccess

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

  -- <INDEX>
  ViewAny Interface (SelectByIndex ix) ->
    case context of
      ContextEmpty ->
        errEmptyContext
      ContextSearch _ results ->
        withIx ix results viewTargetGroup
      ContextModule mod -> do
        withIx ix (mDeclarations mod) viewDeclarationWithLink
      ContextPackage package ->
        withModuleFromPackageIx ix package viewModuleInterface

  -- /<PREFIX>
  ViewAny Interface (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs viewTargetGroup
      ContextModule m    -> withPrefix pre (mDeclarations m) viewDeclarationWithLink
      ContextPackage p   -> withPrefix pre (pModules p) $ \m ->
          withModuleFromPackage m p viewModuleInterface

  -- :documentation
  ViewAny Documentation SelectContext ->
    case context of
      ContextEmpty            -> errEmptyContext
      ContextSearch _ results -> viewSearchResults results
      ContextModule mod       -> viewModuleDocs mod
      ContextPackage package  -> viewPackageDocs package

  -- :documentation <TERM>
  ViewAny Documentation (Search term) ->
    withFirstSearchResult moduleResult term $ \hmod ->
    withModule (Hoogle.mUrl hmod) viewModuleDocs

  -- :documentation <INDEX>
  ViewAny Documentation (SelectByIndex ix) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs targetGroupDocumentation
      ContextModule m    -> withDeclFromModuleIx ix m viewDeclaration
      ContextPackage p   -> withModuleFromPackageIx ix p viewModuleDocs

  -- :documentation /<PREFIX>
  ViewAny Documentation (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs targetGroupDocumentation
      ContextModule m    -> withPrefix pre (mDeclarations m) viewDeclaration
      ContextPackage p   -> withPrefix pre (pModules p) $ \m ->
          withModuleFromPackage m p viewModuleDocs
  -- :src
  ViewDeclarationSource SelectContext ->
    throwError "no declaration selected. Use ':src INT'"

  -- :src <TERM>
  ViewDeclarationSource (Search term) ->
    withFirstSearchResult declResult term (viewSource . Hoogle.dUrl)

  -- :src <INDEX>
  ViewDeclarationSource (SelectByIndex ix) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs $
          maybe errNoSourceAvailable (viewSource . Hoogle.dUrl) . toDecl . NonEmpty.head
      ContextModule m    -> withDeclFromModuleIx ix m (viewSource . declUrl)
      ContextPackage _   -> errSourceOnlyForDeclarations

  -- :src <INDEX>
  ViewDeclarationSource (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs $
          maybe errNoSourceAvailable (viewSource . Hoogle.dUrl) . toDecl . NonEmpty.head
      ContextModule m    -> withPrefix pre (mDeclarations m) (viewSource . declUrl)
      ContextPackage _   -> errSourceOnlyForDeclarations

  -- :declaration
  ViewDeclaration SelectContext ->
    throwError "no declaration selected."

  -- :declaration <TERM>
  ViewDeclaration (Search term) ->
    withFirstSearchResult declResult term $ \hdecl ->
    viewInTerminalPaged $ viewDescription (Hoogle.Declaration hdecl)

  -- :declaration <INDEX>
  ViewDeclaration (SelectByIndex ix) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs $ viewInTerminalPaged . viewDescription . NonEmpty.head
      ContextModule m    -> withDeclFromModuleIx ix m viewDeclaration
      ContextPackage _   -> errNotDeclarationButModule

  -- :declaration /<prefix>
  ViewDeclaration (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs $ viewInTerminalPaged . viewDescription . NonEmpty.head
      ContextModule m    -> withPrefix pre (mDeclarations m) viewDeclaration
      ContextPackage _   -> errNotDeclarationButModule

  -- :minterface
  -- :mdocumentation
  ViewModule view SelectContext ->
    case context of
      ContextModule mod -> viewModule view mod
      _                 -> throwError "not in a module context"

  -- :minterface <TERM>
  -- :mdocumentation <TERM>
  ViewModule view (Search term) ->
    withFirstSearchResult moduleResult term $ \hmod ->
    withModule (Hoogle.mUrl hmod) $ \mod ->
    viewModule view mod

  -- :minterface <INDEX>
  -- :mdocumentation <INDEX>
  ViewModule view (SelectByIndex ix) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs $ withModuleForTargetGroup $ viewModule view
      ContextModule m    -> viewModule view m
      ContextPackage p   -> withModuleFromPackageIx ix p (viewModule view)

  -- :minterface /<PREFIX>
  -- :mdocumentation /<PREFIX>
  ViewModule view (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs $ withModuleForTargetGroup $ viewModule view
      ContextModule m    -> viewModule view m
      ContextPackage p   -> withPrefix pre (pModules p) $ \mod ->
        withModuleFromPackage mod p (viewModule view)

  -- :pinterface
  -- :pdocumentation
  ViewPackage view SelectContext ->
    case context of
      ContextPackage package ->
        viewPackage view package
      ContextModule mod ->
        withPackageForModule mod (viewPackage view)
      _ -> throwError "not in a package context"

  -- :pinterface <TERM>
  -- :pdocumentation <TERM>
  ViewPackage view (Search term) -> do
    let url = hooglePackageUrl term
    html <- fetch' url
    let package = parsePackageDocs url html
    State.modify' $ \s -> s{ sContext = ContextPackage package }
    viewPackage view package

  -- :pinterface <INDEX>
  -- :pdocumentation <INDEX>
  ViewPackage view (SelectByIndex ix) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs $ withPackageForTargetGroup (viewPackage view)
      ContextModule m    -> withPackageForModule m (viewPackage view)
      ContextPackage p   -> viewPackage view p

  -- :pinterface /<PREFIX>
  -- :pdocumentation /<PREFIX>
  ViewPackage view (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs $ withPackageForTargetGroup $ viewPackage view
      ContextModule m    -> withPackageForModule m (viewPackage view)
      ContextPackage p   -> viewPackage view p

targetGroupDocumentation :: TargetGroup -> M ()
targetGroupDocumentation tgroup = do
  let item = NonEmpty.head tgroup
  context <- State.gets sContext
  case item of
    Hoogle.Module hmod ->
      withModule (Hoogle.mUrl hmod) viewModuleDocs
    Hoogle.Package pkg ->
      withPackage (Hoogle.pUrl pkg) viewPackageDocs
    Hoogle.Declaration d ->
      withModule (Hoogle.dModuleUrl d) $ \mod -> do
      State.modify' $ \ s -> s { sContext = context }
      viewInTerminalPaged $ case targetDeclaration d mod of
        Just decl -> prettyDecl decl
        Nothing   -> viewDescription item

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

errNotDeclarationButModule :: M a
errNotDeclarationButModule =
  throwError "item at index is not a declaration; it is a module."

targetDeclaration :: Hoogle.Declaration -> Module -> Maybe Declaration
targetDeclaration decl = lookupDecl anchor
  where
    DeclUrl _ anchor = Hoogle.dUrl decl

withModule
  :: ModuleUrl
  -> (Module -> M a)
  -> M a
withModule url act = do
  html <- fetch' url
  let mod = parseModuleDocs url html
  State.modify' $ \s -> s{ sContext = ContextModule mod }
  act mod

withPackage :: PackageUrl -> (Package -> M a) -> M a
withPackage url act = do
  html <- fetch' url
  let package = parsePackageDocs url html
  State.modify' $ \s -> s{ sContext = ContextPackage package }
  act package

withPackageForModule :: Module -> (Package -> M a) -> M a
withPackageForModule mod act = do
  let url = packageUrl $ mUrl mod
  html <- fetch' url
  let package = parsePackageDocs url html
  State.modify' $ \s -> s{ sContext = ContextPackage package }
  act package

-- | Get an element matching a prefix
withPrefix :: HasCompletion a => String -> [a] -> (a -> M b) -> M b
withPrefix prefix values act =
  case find ((prefix `isPrefixOf`) . completion) values of
    Nothing -> throwError "No item matching prefix"
    Just res -> act res

-- | Get an element from a one-indexed index
withIx :: Int -> [a] -> (a -> M b) -> M b
withIx ix xs act =
  maybe (throwError "index out of range") act
  $ listToMaybe
  $ drop (ix - 1) xs

withPackageForTargetGroup :: (Package -> M a) -> TargetGroup -> M a
withPackageForTargetGroup act tgroup = do
  purl <- selectPackage tgroup
  withPackage purl act
  where
    selectPackage :: TargetGroup -> M PackageUrl
    selectPackage
      = promptSelectOne
      . NonEmpty.fromList
      . nubBy ((==) `on` fst)
      . mapMaybe f
      . toList

    f :: Hoogle.Item -> Maybe (PackageUrl, P.Doc)
    f x = case x of
      Hoogle.Module m      -> (Hoogle.mPackageUrl m,) <$> viewItemPackage x
      Hoogle.Declaration d -> (Hoogle.dPackageUrl d,) <$> viewItemPackage x
      Hoogle.Package p     -> (Hoogle.pUrl p,)        <$> viewItemPackage x

withModuleForTargetGroup :: (Module -> M a) -> TargetGroup -> M a
withModuleForTargetGroup act tgroup = do
  murl <- selectModule tgroup
  withModule murl act
  where
    selectModule :: TargetGroup -> M ModuleUrl
    selectModule
      = promptSelectOne
      . NonEmpty.fromList
      . mapMaybe f
      . toList

    f :: Hoogle.Item -> Maybe (ModuleUrl, P.Doc)
    f x = case x of
      Hoogle.Module m ->
        (Hoogle.mUrl m,) <$> viewItemPackageAndModule x
      Hoogle.Declaration d ->
        (Hoogle.dModuleUrl d,) <$> viewItemPackageAndModule x
      Hoogle.Package _ ->
        Nothing

promptSelectOne :: NonEmpty (a, P.Doc) -> M a
promptSelectOne nonEmptyXs
  | [(x,_)] <- toList nonEmptyXs = return x
  | xs      <- toList nonEmptyXs = do
    liftIO $ putStrLn "Select one:"
    viewInTerminal $ P.vsep $ numbered $ map snd xs
    num <- getInputLine ": "
    case readMaybe =<< num of
      Just n -> case listToMaybe $ drop (n - 1) xs of
        Just (x, _) -> return x
        Nothing -> do
          liftIO $ putStrLn "Invalid index"
          promptSelectOne nonEmptyXs
      Nothing -> do
        liftIO $ putStrLn "Number not recognised"
        promptSelectOne nonEmptyXs

withModuleFromPackage :: String -> Package -> (Module -> M a) -> M a
withModuleFromPackage modName Package{..} act = do
  let url = packageModuleUrl pUrl modName
  html <- fetch' url
  let mod = parseModuleDocs url html
  State.modify' $ \s -> s{ sContext = ContextModule mod }
  act mod

withModuleFromPackageIx :: Int -> Package -> (Module -> M a) -> M a
withModuleFromPackageIx ix p act =
  withIx ix (pModules p) $ \m -> withModuleFromPackage m p act

withDeclFromModuleIx :: Int -> Module -> (Declaration -> M a) -> M a
withDeclFromModuleIx ix = withIx ix . mDeclarations

viewSearchResults :: MonadIO m => [TargetGroup] -> m ()
viewSearchResults
  = viewInTerminal
  . P.vsep
  . reverse
  . numbered
  . map viewSummary

viewDeclaration :: MonadIO m => Declaration -> m ()
viewDeclaration = viewInTerminalPaged . prettyDecl

viewDeclarationWithLink :: MonadIO m => Declaration -> m ()
viewDeclarationWithLink decl = viewInTerminalPaged $ P.vcat
  [ prettyDecl decl
    -- ad-hoc link colour
  , P.cyan $ P.text $ getUrl (dDeclUrl decl)
  ]

viewModule :: MonadIO m => View -> Module -> m ()
viewModule Interface = viewModuleInterface
viewModule Documentation = viewModuleDocs

viewModuleInterface :: MonadIO m => Module -> m ()
viewModuleInterface
  = viewInTerminalPaged
  . P.vsep
  . numbered
  . map (prettyHtml . dSignature)
  . mDeclarations

viewModuleDocs :: MonadIO m => Module -> m ()
viewModuleDocs = viewInTerminalPaged . prettyModule

viewPackage :: MonadIO m => View -> Package -> m ()
viewPackage Interface = viewPackageInterface
viewPackage Documentation = viewPackageDocs

viewPackageInterface :: MonadIO m => Package -> m ()
viewPackageInterface Package{..} =
  viewInTerminalPaged $ P.vsep $ numbered (P.text <$> pModules)

viewPackageDocs :: MonadIO m => Package -> m ()
viewPackageDocs Package{..} = viewInTerminalPaged $ P.vsep $ catMaybes
  [ Just $ mainHeading $ case pSubTitle of
      Nothing -> pTitle
      Just s -> pTitle <> ": " <> s
  , Just $ section "Description" (prettyHtml pDescription)
  , section "Readme" . prettyHtml <$> pReadme
  , Just $ section "Properties" (P.vsep $ map viewProp pProperties)
  ]
  where
    section heading body =
      P.text heading <> P.nest 2 (P.linebreak <> body)

    viewProp (title, body) =
      section title (prettyHtml body)


viewInTerminal :: MonadIO m => P.Doc -> m ()
viewInTerminal = printDoc stdout

viewInTerminalPaged :: MonadIO m => P.Doc -> m ()
viewInTerminalPaged doc = withPager $ \handle -> printDoc handle doc

withPager :: MonadIO m => (Handle -> IO ())  -> m ()
withPager act = liftIO $
  MVar.newEmptyMVar >>= \mvar ->
  Async.withAsync (runPager mvar) $ \pager ->
  Async.withAsync (runAction mvar) $ \action -> do
    res <- Async.waitEitherCatch pager action
    case res of
      -- pager finished first. Action aborted.
      Left (Right _) -> return ()
      Left (Left err) -> throwIO err
      -- action finished first. Wait for pager.
      Right (Right _) -> void $ Async.waitCatch pager
      Right (Left err) -> throwIO err
  where
    cmd = (Process.proc "less" ["-iFRX"]) { Process.std_in = Process.CreatePipe }
    runAction mvar = do
      handle <- MVar.readMVar mvar
      act handle `finally` hClose handle

    runPager mvar =
      Process.withCreateProcess cmd
         $ \(Just hin) _ _ p -> do
           MVar.putMVar mvar hin
           Process.waitForProcess p

-- | Maximum screen width for flowing text.
-- Fixed-width portions will still overflow that.
maxWidth :: Int
maxWidth = 80

printDoc :: MonadIO m => Handle -> P.Doc -> m ()
printDoc handle doc = liftIO $ do
  width <- min maxWidth . maybe maxWidth Terminal.width <$> Terminal.size
  P.displayIO handle $ P.renderSmart 1 width doc
  hPutStrLn handle ""

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

fetch' :: HasUrl a => a -> M HtmlPage
fetch' x = do
  req <- liftIO $ Http.parseRequest $ dropAnchor $ getUrl x
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
      res <- liftIO $ Http.httpLbs req manager
      let status = Http.responseStatus res
      unless (Http.statusIsSuccessful status) $
        throwError
          $ "unable to fetch page: "
          <> Text.unpack (Text.decodeUtf8 $ Http.statusMessage status)
      let body = Http.responseBody res
      liftIO $ MVar.putMVar mvar body
      return body

moduleResult :: (String, Hoogle.Item -> Maybe Hoogle.Module)
moduleResult = ("module", toModule)
  where
    toModule = \case
      Hoogle.Module m -> Just m
      _               -> Nothing

declResult :: (String, Hoogle.Item -> Maybe Hoogle.Declaration)
declResult = ("declaration", toDecl)

toDecl :: Hoogle.Item -> Maybe Hoogle.Declaration
toDecl = \case
  Hoogle.Declaration d -> Just d
  _                    -> Nothing

-- ================================
-- Pretty printing
-- ================================

mainHeading :: String -> P.Doc
mainHeading str = P.vsep
  [ divider
  , P.text str
  , divider
  ]
  where
    divider = P.text $ replicate maxWidth '='

viewDescription :: Hoogle.Item -> P.Doc
viewDescription = prettyHtml . Hoogle.description

viewSummary :: TargetGroup -> P.Doc
viewSummary tgroup = P.vsep
  [ viewDescription $ NonEmpty.head tgroup
  , viewPackageInfoList tgroup
  ]

viewPackageInfoList :: TargetGroup -> P.Doc
viewPackageInfoList
  = P.group
  . P.fillSep
  . P.punctuate P.comma
  . mapMaybe viewItemPackageAndModule
  . toList

viewPackageName :: String -> P.Doc
viewPackageName = P.magenta . P.text

viewModuleName :: String -> P.Doc
viewModuleName = P.black . P.text

viewItemPackage :: Hoogle.Item -> Maybe P.Doc
viewItemPackage = \case
  Hoogle.Declaration d ->
    Just $ viewPackageName (Hoogle.dPackage d)
  Hoogle.Module m ->
    Just $ viewPackageName (Hoogle.mPackage m)
  Hoogle.Package _ ->
    Nothing

viewItemPackageAndModule :: Hoogle.Item -> Maybe P.Doc
viewItemPackageAndModule = \case
  Hoogle.Declaration d ->
    Just $ viewPackageName (Hoogle.dPackage d)
      P.<+> viewModuleName (Hoogle.dModule d)
  Hoogle.Module m ->
    Just $ viewPackageName (Hoogle.mPackage m)
  Hoogle.Package _ ->
    Nothing

prettyModule :: Module -> P.Doc
prettyModule (Module name minfo decls _) =
  P.vsep $ [mainHeading name]
    ++ [ prettyHtml info | Just info <- [minfo] ]
    ++ [ prettyDecl decl | decl <- decls ]

prettyDecl :: Declaration -> P.Doc
prettyDecl Declaration{..} =
  P.vsep $ map prettyHtml (dSignature:dContent)

lookupDecl :: Anchor -> Module -> Maybe Declaration
lookupDecl anchor (Module _ _ decls _) =
  find (Set.member anchor . dAnchors) decls

viewTargetGroup :: TargetGroup -> M ()
viewTargetGroup tgroup = viewInTerminalPaged $ P.vsep
  [ divider
  , content
  , divider
  ]
  where
    divider = P.black $ P.text $ replicate 50 '='
    representative = NonEmpty.head tgroup
    toUrl = \case
      Hoogle.Declaration d -> getUrl $ Hoogle.dUrl d
      Hoogle.Module      m -> getUrl $ Hoogle.mUrl m
      Hoogle.Package     p -> getUrl $ Hoogle.pUrl p
    content = P.vsep $
      [ viewDescription representative
      , viewPackageInfoList tgroup
      , prettyHtml $ Hoogle.docs representative
      ] ++ reverse (P.cyan . P.text . toUrl <$> toList tgroup)

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

declUrl :: Declaration -> DeclUrl
declUrl Declaration{..} =  DeclUrl dModuleUrl dAnchor

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

