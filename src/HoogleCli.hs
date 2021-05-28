{-# LANGUAGE LambdaCase #-}
module HoogleCli
  ( interactive
  , evaluate
  , ShellState(..)
  , Context(..)
  , Cmd(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Foldable (toList)
import Data.Functor (void, (<&>))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.List hiding (groupBy)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import System.Environment (getEnv)
import System.IO (stdout, Handle)
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
  | ContextModule ModuleDocs            -- ^ looking at module docs

type Index = Int

-- | Commands we accept
data Cmd
  = Search String
  | ViewContext                  -- ^ show data from current context
  | ViewDocs Index               -- ^ docs provided in the hoogle response
  | ViewExtendedDocs Index       -- ^ declaration's docs available in the haddock page
  | ViewModuleDocs (Maybe Index) -- ^ full haddock for module
  | ViewModuleInterface Index    -- ^ all function signatures
  | ViewSource Index             -- ^ source for target
  | ViewPackageModules Index     -- ^ list modules from a package
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
    case cmd of
        "src" -> intCmd ViewSource
        "module-doc"
          | null args -> Right $ ViewModuleDocs Nothing
          | otherwise -> intCmd (ViewModuleDocs . Just)
        "edoc" -> intCmd ViewExtendedDocs
        "interface" -> intCmd ViewModuleInterface
        "package" -> intCmd ViewPackageModules
        "quit" -> Right Quit
        _ -> error $ "Unknown command: " <> cmd
  x | Just n <- readMaybe x -> Right (ViewDocs n)
  [] -> Right ViewContext
  _ -> Right $ Search str

interactive :: M ()
interactive = do
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
      ContextEmpty -> liftIO $ putStrLn "no context"
      ContextSearch term results -> do
        viewSearchResults results
        liftIO $ putStrLn $ "search: " <> term
      ContextModule mdocs -> do
        viewModuleInterface mdocs
        liftIO $ putStrLn $ "module: " <> mTitle mdocs
  Search str -> do
    res <- toGroups <$> runSearch str
    viewSearchResults res
    State.modify' $ \s -> s
        { sContext = ContextSearch str res
        }
  ViewDocs ix -> do
    context <- State.gets sContext
    case context of
      ContextEmpty -> liftIO $ putStrLn "no context"
      ContextSearch _ _ ->
        getTargetGroup ix $ \tgroup -> do
        viewInTerminal $ viewFull tgroup
      ContextModule mdocs ->
        viewInTerminal $ prettyDecl $ mDeclarations mdocs !! (ix - 1)
  ViewExtendedDocs ix ->
    getTargetGroup ix $ \tgroup -> do
    let target = NonEmpty.head tgroup
    let ModuleLink url manchor = moduleLink target
    html <- fetch' url
    let modl = parseModuleDocs url html
        desc = case manchor of
          Nothing -> prettyHtml <$> mDescription modl
          Just an -> prettyDecl <$> lookupDecl an modl
    viewInTerminal $ fromMaybe mempty desc
  ViewModuleInterface ix ->
    getTargetGroup ix $ \tgroup -> do
    target <- promptSelectOne tgroup
    let ModuleLink url _ = moduleLink target
    html <- fetch' url
    let modl = parseModuleDocs url html
    State.modify' $ \s -> s
        { sContext = ContextModule modl
        }
    viewModuleInterface modl
  ViewModuleDocs Nothing ->
    withModuleContext viewModuleDocs
  ViewModuleDocs (Just ix) ->
    getTargetGroup ix $ \tgroup -> do
    target <- promptSelectOne tgroup
    let ModuleLink url _ = moduleLink target
    html <- fetch' url
    viewModuleDocs (parseModuleDocs url html)
  ViewSource ix ->
    getTargetGroup ix $ \tgroup -> do
    target <- promptSelectOne tgroup
    editSource target
  ViewPackageModules ix -> do
    context <- State.gets sContext
    case context of
      ContextEmpty -> liftIO $ putStrLn "no package to show"
      ContextSearch _ _ -> getTargetGroup ix undefined
      ContextModule _ -> undefined


withSearchContext :: ([TargetGroup] -> M ()) -> M ()
withSearchContext f = do
  context <- State.gets sContext
  case context of
    ContextSearch _ results -> f results
    _ -> liftIO $ putStrLn "No search results available"

withModuleContext :: (ModuleDocs -> M ()) -> M ()
withModuleContext f = do
  context <- State.gets sContext
  case context of
    ContextModule mdocs -> f mdocs
    _ -> liftIO $ putStrLn "No module selected"

getTargetGroup :: Int -> (TargetGroup -> M ()) -> M ()
getTargetGroup ix f =
  withSearchContext $ f . (!! (ix - 1))

viewSearchResults :: MonadIO m => [TargetGroup] -> m ()
viewSearchResults
  = viewInTerminal
  . P.vsep
  . reverse
  . numbered
  . map viewCompact

viewModuleInterface :: MonadIO m => ModuleDocs -> m ()
viewModuleInterface
  = viewInTerminal
  . P.vsep
  . numbered
  . map prettyHtml
  . map dSignature
  . mDeclarations

viewModuleDocs :: MonadIO m => ModuleDocs -> m ()
viewModuleDocs = viewInEditor . prettyModule

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

viewInEditor :: MonadIO m => P.Doc -> m ()
viewInEditor doc = liftIO $ do
  withSystemTempFile "doc" $ \fullpath handle -> do
    printDoc handle $ P.plain doc
    editor <- getEditor
    void $ Process.system (editor ++ " " ++ fullpath)

printDoc :: MonadIO m => Handle -> P.Doc -> m ()
printDoc handle doc = liftIO $ do
  width <- maybe 80 Terminal.width <$> Terminal.size
  P.displayIO handle $ P.renderSmart 1 width doc
  putStrLn ""

editSource :: Hoogle.Target -> M ()
editSource target = do
  mlink <- sourceLink target
  case mlink of
    Nothing -> liftIO $ putStrLn "no source for file"
    Just link -> do
      html <- fetch' link
      view (fileInfo link html)

view :: FileInfo -> M ()
view (FileInfo filename mline content) = do
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
instance HasUrl ModuleLink where getUrl (ModuleLink url _) = url
instance HasUrl SourceLink where getUrl (SourceLink url _) = url
instance HasUrl Url        where getUrl url = url

fetch' :: HasUrl a => a -> M Html
fetch' x = do
  req <- liftIO $ Http.parseRequest $ getUrl x
  src <- fetch req
  return (parseHtml src)

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

prettyModule :: ModuleDocs -> P.Doc
prettyModule (ModuleDocs name minfo decls _) =
  P.vsep $ [title]
    ++ [ prettyHtml info | Just info <- [minfo] ]
    ++ [ prettyDecl decl | decl <- decls ]
  where
    title = P.vsep
      [ P.text name
      , P.text $ replicate (length name) '='
      , ""
      ]

prettyDecl :: DeclarationDocs -> P.Doc
prettyDecl DeclarationDocs{..} =
  P.vsep $ map prettyHtml $ dSignature:dContent

lookupDecl :: Anchor -> ModuleDocs -> Maybe DeclarationDocs
lookupDecl anchor (ModuleDocs _ _ decls _) =
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
  let mlink@(ModuleLink _ manchor) = moduleLink target
  case manchor of
    Nothing -> return Nothing
    Just anchor -> do
      html <- fetch' mlink
      let links = sourceLinks mlink html
      case lookup anchor links of
        Nothing -> error $ unlines $
          [ "anchor missing in module docs"
          , show mlink
          ] ++ map show links
        Just link -> return $ Just link

-- | Get URL for module documentation
moduleLink :: Hoogle.Target -> ModuleLink
moduleLink target = ModuleLink (dropAnchor url) (takeAnchor url)
  where
    url = Hoogle.targetURL target

