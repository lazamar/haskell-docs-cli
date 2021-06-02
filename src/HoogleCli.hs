{-# LANGUAGE LambdaCase #-}
module HoogleCli
  ( interactive
  , evaluate
  , ShellState(..)
  , Context(..)
  , Cmd(..)
  , packageUrl
  ) where

import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad.Trans.Class (lift)
import Control.Monad ((<=<))
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.State.Lazy (StateT)
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
import qualified Control.Monad.Trans.State.Lazy as State
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
  = ViewContext                      -- ^ show data from current context
  | Select Index                     -- ^ view item from context
  | Search String                    -- ^ Hoogle search
  | ViewSource Index                 -- ^ source for target
  | ViewExtendedDocs Index           -- ^ declaration's docs available in the haddock page
  | ViewModuleDocs (Maybe Index)     -- ^ full haddock for module
  | ViewModuleInterface Index        -- ^ all function signatures
  | ViewPackageModules               -- ^ list modules from a package
      (Maybe (Either String Index))
  | Quit

type M a = StateT ShellState (CLI.InputT IO) a

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
  [ "src"
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
        mIntCmd f
          | null args = Right (f Nothing)
          | otherwise = intCmd (f . Just)
    case cmd of
        "src" -> intCmd ViewSource
        "module-doc" -> mIntCmd ViewModuleDocs
        "edoc" -> intCmd ViewExtendedDocs
        "interface" -> intCmd ViewModuleInterface
        "package"
          | null args
          -> Right $ ViewPackageModules Nothing
          | Just n <- readMaybe args
          -> Right $ ViewPackageModules $ Just $ Right n
          | otherwise
          -> Right $ ViewPackageModules $ Just $ Left args

        "quit" -> Right Quit
        _ -> error $ "Unknown command: " <> cmd
  x | Just n <- readMaybe x -> Right (Select n)
  [] -> Right ViewContext
  _ -> Right $ Search str

interactive :: M ()
interactive = do
  context <- State.gets sContext
  case context of
    ContextEmpty      -> return ()
    ContextSearch t _ -> liftIO $ putStrLn $ "search: " <> t
    ContextModule m   -> liftIO $ putStrLn $ "module: " <> mTitle m
    ContextPackage p  -> liftIO $ putStrLn $ "package: " <> pTitle p
  minput <- lift $ CLI.getInputLine "hoogle> "
  case parseCommand $ fromMaybe "" minput of
    Left err -> liftIO (putStrLn err) >> interactive
    Right Quit -> return ()
    Right cmd -> evaluate cmd >> interactive

evaluate :: Cmd -> M ()
evaluate = \case
  Quit -> error "impossible"
  ViewContext -> do
    context <- State.gets sContext
    case context of
      ContextEmpty            -> return ()
      ContextSearch _ results -> viewSearchResults results
      ContextModule mdocs     -> viewModuleInterface mdocs
      ContextPackage package  -> viewPackageModules package
  Search str -> do
    res <- toGroups <$> runSearch str
    viewSearchResults res
    State.modify' $ \s -> s{ sContext = ContextSearch str res }
  Select ix -> do
    context <- State.gets sContext
    case context of
      ContextEmpty -> fail "no context"
      ContextSearch _ _ -> getTargetGroup ix (viewInTerminalPaged . viewFull)
      ContextModule module' -> do
        decl <- elemAt ix (mDeclarations module')
        viewInTerminalPaged $ P.vsep
          [ prettyDecl decl
            -- ad-hoc link colour
          , P.cyan $ P.text $ getUrl (dModuleUrl decl)
          ]
      ContextPackage (Package _ modules purl) -> do
        url <- packageModuleUrl purl <$> elemAt ix modules
        html <- fetch' url
        let modl = parseModuleDocs url html
        State.modify' $ \s -> s{ sContext = ContextModule modl }
        viewModuleInterface modl
  ViewExtendedDocs ix ->
    getTargetGroup ix $ \tgroup -> do
    let target = NonEmpty.head tgroup
        url = moduleUrl target
    html <- fetch' url
    let modl = parseModuleDocs url html
        desc = case declUrl target of
          Nothing -> prettyHtml <$> mDescription modl
          Just (DeclUrl _ anchor) -> prettyDecl <$> lookupDecl anchor modl
    viewInTerminal $ fromMaybe mempty desc
  ViewModuleInterface ix ->
    getTarget ix $ \target -> do
    let url = moduleUrl target
    html <- fetch' url
    let modl = parseModuleDocs url html
    State.modify' $ \s -> s{ sContext = ContextModule modl }
    viewModuleInterface modl
  ViewModuleDocs Nothing ->
    withModuleContext viewModuleDocs
  ViewModuleDocs (Just ix) ->
    getTarget ix $ \target -> do
    let url = moduleUrl target
    html <- fetch' url
    viewModuleDocs (parseModuleDocs url html)
  ViewSource ix ->
    getTarget ix viewSource
  ViewPackageModules (Just (Left term)) -> do
    let url = hooglePackageUrl term
    html <- fetch' url
    let package = parsePackageDocs url html
    viewPackageModules package
    State.modify' $ \s -> s{ sContext = ContextPackage package }
  ViewPackageModules msearch -> do
    context <- State.gets sContext
    package <- case context of
      ContextEmpty -> fail "no package to show"
      ContextSearch _ _
        | Just (Right ix) <- msearch ->
          getTarget ix $ \target -> do
          let url = packageUrl $ moduleUrl target
          html <- fetch' url
          let package = parsePackageDocs url html
          viewPackageModules package
          return package
        | otherwise -> fail "no option selected"
      ContextModule module' -> do
        let url = packageUrl $ mUrl module'
        html <- fetch' url
        let package = parsePackageDocs url html
        viewPackageModules package
        return package
      ContextPackage package -> do
        viewPackageModules package
        return package
    State.modify' $ \s -> s{ sContext = ContextPackage package }

-- | Get an element from a one-indexed index
elemAt :: Int -> [a] -> M a
elemAt ix =
  maybe (fail "Index out of range") return
  . listToMaybe
  . drop (ix - 1)

withSearchContext :: ([TargetGroup] -> M a) -> M a
withSearchContext f = do
  context <- State.gets sContext
  case context of
    ContextSearch _ results -> f results
    _ -> fail "No search results available"

withModuleContext :: (Module -> M a) -> M a
withModuleContext f = do
  context <- State.gets sContext
  case context of
    ContextModule mdocs -> f mdocs
    _ -> fail "No module selected"

getTarget :: Int -> (Hoogle.Target -> M a) -> M a
getTarget ix f = getTargetGroup ix (f <=< promptSelectOne)

getTargetGroup :: Int -> (TargetGroup -> M a) -> M a
getTargetGroup ix f = withSearchContext $ \results -> do
  el <- elemAt ix results
  f el

viewSearchResults :: MonadIO m => [TargetGroup] -> m ()
viewSearchResults
  = viewInTerminal
  . P.vsep
  . reverse
  . numbered
  . map viewCompact

viewModuleInterface :: MonadIO m => Module -> m ()
viewModuleInterface
  = viewInTerminalPaged
  . P.vsep
  . numbered
  . map (prettyHtml . dSignature)
  . mDeclarations

viewModuleDocs :: MonadIO m => Module -> m ()
viewModuleDocs = viewInTerminalPaged . prettyModule

viewPackageModules :: MonadIO m => Package -> m ()
viewPackageModules (Package _ modules _) =
  viewInTerminal $ P.vsep $ numbered (P.text <$> modules)

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

      num <- lift $ CLI.getInputLine ": "
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
  let cmd = (Process.proc "less" ["-eFRX"]) { Process.std_in = Process.CreatePipe }
  Process.withCreateProcess cmd
     $ \(Just hin) _ _ p -> do
       res <- act hin `finally` hClose hin
       _   <- Process.waitForProcess p
       return res

printDoc :: MonadIO m => Handle -> P.Doc -> m ()
printDoc handle doc = liftIO $ do
  width <- maybe 80 Terminal.width <$> Terminal.size
  P.displayIO handle $ P.renderSmart 1 width doc
  putStrLn ""

viewSource :: Hoogle.Target -> M ()
viewSource target = do
  mlink <- sourceLink target
  case mlink of
    Nothing -> liftIO $ putStrLn "no source available"
    Just link -> do
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

-- ================================
-- Pretty printing
-- ================================

viewItem :: Hoogle.Target -> P.Doc
viewItem = prettyHtml . parseHoogleHtml . Hoogle.targetItem

viewCompact :: TargetGroup -> P.Doc
viewCompact tgroup = P.vsep
  [ viewItem $ NonEmpty.head tgroup
  , viewPackageInfoList tgroup
  ]

viewPackageInfoList :: TargetGroup -> P.Doc
viewPackageInfoList
  = P.vsep
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
sourceLink :: Hoogle.Target -> M (Maybe SourceLink)
sourceLink target = do
  case declUrl target of
    Nothing -> return Nothing
    Just (DeclUrl murl anchor) -> do
      html <- fetch' murl
      let links = sourceLinks murl html
      case lookup anchor links of
        Nothing -> error $ unlines $
          [ "anchor missing in module docs"
          , show murl
          ] ++ map show links
        Just link -> return $ Just link

-- | Get URL for module documentation
moduleUrl :: Hoogle.Target -> ModuleUrl
moduleUrl target = ModuleUrl (dropAnchor url)
  where
    url = Hoogle.targetURL target

declUrl :: Hoogle.Target -> Maybe DeclUrl
declUrl target = do
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

