{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wwarn #-}
module Docs.CLI.Evaluate
  ( interactive
  , evaluate
  , evaluateCmd
  , ShellState(..)
  , Context(..)
  , Cmd(..)
  , Selection(..)
  , View(..)
  , HackageUrl(..)
  , HoogleUrl(..)
  , runCLI
  , defaultHoogleUrl
  , defaultHackageUrl
  , moreInfoText
  ) where

import Prelude hiding (mod)
import Control.Applicative ((<|>))
import Control.Exception (finally, throwIO, try)
import Control.Monad (unless, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT(..), MonadError, catchError, runExceptT, throwError)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Lazy (MonadState, StateT(..))
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.List hiding (groupBy)
import Data.List.Extra (breakOn)
import Data.Char (isSpace)
import System.Environment (getEnv, lookupEnv)
import System.IO (hPutStrLn, hClose, hFlush, stdout, Handle, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Exit (exitSuccess)
import qualified Hoogle as H
import System.FilePath ((</>))
import Network.URI (uriToString)

import Docs.CLI.Types
import Docs.CLI.Haddock as Haddock
import qualified Docs.CLI.Hoogle as Hoogle
import Data.Cache

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
  , sCache :: Cache
  , sNoColours :: Bool
  , sHoogle :: HoogleUrl
  , sHackage :: HackageUrl
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
  | Help
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

newtype HoogleUrl = HoogleUrl Url

newtype HackageUrl = HackageUrl Url

defaultHoogleUrl :: HoogleUrl
defaultHoogleUrl =
  HoogleUrl "https://hoogle.haskell.org"

defaultHackageUrl :: HackageUrl
defaultHackageUrl =
  HackageUrl "https://hackage.haskell.org"

runCLI :: ShellState -> M a -> IO (Either String a)
runCLI state program = do
  settings <- cliSettings
  flip State.evalStateT state
    $ CLI.runInputT settings
    $ CLI.withInterrupt
    $ runExceptT
    $ runM program

cliSettings :: IO (CLI.Settings (StateT ShellState IO))
cliSettings = do
  mhome <- lookupEnv "HOME"
  return $ def
    { CLI.complete = complete
    , CLI.historyFile = mhome <&> (</> ".hasekell-docs-cli.history")
    }
  where
    def :: CLI.Settings (StateT ShellState IO)
    def = CLI.defaultSettings

complete :: CLI.CompletionFunc (StateT ShellState IO)
complete (left', _) = do
  let left = reverse left'
  context <- State.gets sContext
  let options = case context of
        ContextEmpty -> []
        ContextSearch _ tgroups -> map (completion . NonEmpty.head) tgroups
        ContextModule m -> map completion (mDeclarations m)
        ContextPackage p -> pModules p

      asCompletion prefix option =
        CLI.Completion
          { CLI.replacement = drop (length prefix) option
          , CLI.display = option
          , CLI.isFinished = True
          }

      dropEnd n = reverse . drop n . reverse

      -- drop from begining til after the infix
      -- quadratic, but only executed in one string so doesn't
      -- matter very much
      dropInfix _ [] = []
      dropInfix inf (_:ys) =
        if inf `isPrefixOf` ys
          then drop (length inf) ys
          else dropInfix inf ys

      completionsFor :: String -> String -> (String , [CLI.Completion])
      completionsFor l xs
        | cs@(_:_) <- filter (xs `isPrefixOf`) options =
          (l, map (asCompletion xs) cs)
        | Just option <- find (xs `isInfixOf`) options =
          let newPrefix = dropEnd (length $ dropInfix xs option) option
              newLeft = reverse newPrefix <> dropWhile (/= '/') l
          in completionsFor newLeft newPrefix
        | otherwise = (l, [])

  return $ case left of
    ':':xs | not (any isSpace xs) , Just cinfo <- cmdInfoFromPrefix xs ->
      (":", [CLI.simpleCompletion $ commandName cinfo])
    ':':xs | (_, ' ':'/':prefix) <- break isSpace xs ->
      completionsFor left' prefix
    '/':xs ->
      completionsFor left' xs
    _ ->
      (left', [])

class MonadCLI m where
  getInputLine :: String -> m (Maybe String)

instance MonadCLI M where
  getInputLine str = M $ lift $ CLI.getInputLine str

runSearch :: String -> M [Hoogle.Item]
runSearch term = do
  HoogleUrl url <- State.gets sHoogle
  req <- Http.parseRequest url
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

packageUrl :: HackageUrl -> String -> PackageUrl
packageUrl (HackageUrl hackage) pname =
  PackageUrl $ hackage ++ "/package/" ++ pname

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

newtype CmdInfo = CmdInfo (String, Selection -> Cmd, P.Doc)

commandName :: CmdInfo -> String
commandName (CmdInfo (name, _,_)) = takeWhile (not . isSpace) name

commands :: [CmdInfo]
commands = map CmdInfo
  --any
  [ ("documentation <selector>",
        ViewAny Documentation,
        "") -- this can come out?
  , ("interface <selector>",
        ViewAny Interface,
        "" )
  , ("src <selector>",
        ViewDeclarationSource,
        "View the source code of a function or type" <> P.linebreak
        <> "Set the editor with the 'EDITOR' environment variable.")
  -- declaration
  , ("declaration <selector>",
        ViewDeclaration,
        "View the Hackage documentation for a function or type")
  , ("ddocumentation <selector>",
        ViewDeclaration,
        "Alias of :declaration")
  -- module
  , ("module <selector>",
        ViewModule Documentation,
        "View documentation for a module matching a selector")
  , ("mdocumentation <selector>",
        ViewModule Documentation,
        "Alias of :module")
  , ("minterface <selector>",
        ViewModule Interface,
        "View a module's interface")
  -- package
  , ("package <selector>",
        ViewPackage Documentation,
        "View documentation for a package matching a selector")
  , ("pdocumentation <selector>",
        ViewPackage Documentation,
        "Alias of :package")
  , ("pinterface <selector>",
        ViewPackage Interface,
        "View a package's interface")
  , ("help",
        const Help,
        "View this help text")
  , ("quit",
        const Quit,
        "Exit the program")
  ]

cmdInfoFromPrefix :: String -> Maybe CmdInfo
cmdInfoFromPrefix v = find (\cmd -> v `isPrefixOf` commandName cmd) commands

parseCommand :: String -> Either String Cmd
parseCommand str = case str of
  (':':xs) -> do
    let (typedCommand, args) = drop 1 <$> break isSpace xs
        selection
          | null args                = SelectContext
          | ('/':prefix) <- args     = SelectByPrefix prefix
          | Just n <- readMaybe args = SelectByIndex n
          | otherwise                = Search args
    case cmdInfoFromPrefix  typedCommand of
      Just (CmdInfo (_, toCmd, _)) -> Right (toCmd selection)
      Nothing -> Left "*** Unknown command. Type :help for help."
  -- no colon cases
  ('/':prefix)              -> Right $ ViewAny Interface $ SelectByPrefix prefix
  x | Just n <- readMaybe x -> Right $ ViewAny Interface $ SelectByIndex n
  []                        -> Right $ ViewAny Interface SelectContext
  _                         -> Right $ ViewAny Interface $ Search str

interactive :: M ()
interactive = do
  viewInTerminal greeting
  loop $ do
    printContext
    input <- fromMaybe ":quit" <$> getInputLine "> "
    evaluate input
  where
    onError = return $ Right ()

    loop action = tryM action >> loop action

    tryM :: M () -> M ()
    tryM = M. ExceptT . CLI.handleInterrupt onError . runExceptT . runM

    printContext = do
      context <- State.gets sContext
      case context of
        ContextEmpty      -> return ()
        ContextSearch t _ -> viewInTerminal $ P.text $ "search: " <> t
        ContextModule m   -> viewInTerminal $ P.text $ "module: " <> mTitle m
        ContextPackage p  -> viewInTerminal $ P.text $ "package: " <> pTitle p

greeting :: P.Doc
greeting = P.vcat
  [ P.black "---- "
      <> P.blue "haskell-docs-cli"
      <> P.black " ----------------------------------------------------------"
  , P.black "Say :help for help and :quit to exit"
  , P.black "--------------------------------------------------------------------------------"
  ]

evaluate :: String -> M ()
evaluate input =
  case parseCommand input of
    Left err   -> liftIO (putStrLn err)
    Right cmd  -> evaluateCmd cmd `catchError` showFailure
  where
    showFailure e = liftIO $ hPutStrLn stderr $ "Failed: "<> e

evaluateCmd :: Cmd -> M ()
evaluateCmd cmd = State.gets sContext >>= \context -> case cmd of
  Help -> viewInTerminal helpText
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
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs viewTargetGroup
      ContextModule m    -> do withIx ix (mDeclarations m) viewDeclarationWithLink
      ContextPackage p   -> withModuleFromPackageIx ix p viewModuleInterface

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
  -- :ddocumentation
  ViewDeclaration SelectContext ->
    throwError "no declaration selected."

  -- :declaration <TERM>
  ViewDeclaration (Search term) ->
    withFirstSearchResult declResult term $ \hdecl ->
    let tgroup = Hoogle.Declaration hdecl NonEmpty.:| []
    in targetGroupDocumentation tgroup

  -- :declaration <INDEX>
  ViewDeclaration (SelectByIndex ix) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withIx ix xs viewTargetGroup
      ContextModule m    -> withDeclFromModuleIx ix m viewDeclaration
      ContextPackage _   -> errNotDeclarationButModule

  -- :declaration /<prefix>
  ViewDeclaration (SelectByPrefix pre) ->
    case context of
      ContextEmpty       -> errEmptyContext
      ContextSearch _ xs -> withPrefix pre xs viewTargetGroup
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
    hackage <- State.gets sHackage
    let url = packageUrl hackage term
    html <- fetchHTML url
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

moreInfoText :: P.Doc
moreInfoText =
  "More info at <https://github.com/lazamar/haskell-docs-cli>"

helpText :: P.Doc
helpText = P.vcat $ concatMap addLine
  [ hcommands
  , hselectors
  , hexamples
  , moreInfoText
  ]
  where
    addLine :: P.Doc -> [P.Doc]
    addLine line = [line, ""]

    showItems :: [(String, P.Doc)] -> P.Doc
    showItems items =
      let maxNameWidth = maximum $ map (length . fst) items in
      P.indent 2 $ P.vcat
        [ P.fillBreak (maxNameWidth + 2) (P.pretty name) P.<+> P.align description
        | (name,description) <- items ]

    hcommands =  P.vcat
      [ "Commands:"
      , showItems [(":" <> cmd, txt) | CmdInfo (cmd,_,txt) <- commands ]
      ]

    hselectors = P.vcat
      [ "Selectors:"
      , showItems
          [ ("<int>", "select an option by index")
          , ("/<str>", "select an option by prefix")
          , ("<str>", "search for an option")
          ]
      ]

    hexamples = P.vcat
      [ "Examples:"
      , showItems
          [ ("takeWhile", "View Hoogle search results for 'takeWhile'")
          , (":package containers", "View package documentation for the 'containers' package")
          , (":module Data.List", "View module documentation for the 'Data.List' module")
          , (":src insertWith", "View the source for the first Hoogle result for 'insertWith'")
          , (":package 2"
            , "View package documentation for the item with index 2 in the" P.</> "current context"
            )
          , (":module /tak"
            , "View module documentation for the first item with prefix" P.</> "'tak' in the current context"
            )
          ]
      ]
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
  html <- fetchHTML url
  let mod = parseModuleDocs url html
  State.modify' $ \s -> s{ sContext = ContextModule mod }
  act mod

withPackage :: PackageUrl -> (Package -> M a) -> M a
withPackage url act = do
  html <- fetchHTML url
  let package = parsePackageDocs url html
  State.modify' $ \s -> s{ sContext = ContextPackage package }
  act package

withPackageForModule :: Module -> (Package -> M a) -> M a
withPackageForModule mod act = do
  let url = toPackageUrl $ mUrl mod
  html <- fetchHTML url
  let package = parsePackageDocs url html
  State.modify' $ \s -> s{ sContext = ContextPackage package }
  act package

-- | Get an element matching a prefix
withPrefix :: HasCompletion a => String -> [a] -> (a -> M b) -> M b
withPrefix pre values act =
  let prefix = reverse $ dropWhile isSpace $ reverse pre
  in
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
      . nubBy ((==) `on` fst)
      . map f
      . toList

    f :: Hoogle.Item -> (PackageUrl, P.Doc)
    f x = case x of
      Hoogle.Module m      -> (Hoogle.mPackageUrl m, viewItemPackage x)
      Hoogle.Declaration d -> (Hoogle.dPackageUrl d, viewItemPackage x)
      Hoogle.Package p     -> (Hoogle.pUrl p       , viewItemPackage x)

withModuleForTargetGroup :: (Module -> M a) -> TargetGroup -> M a
withModuleForTargetGroup act tgroup = do
  murl <- selectModule tgroup
  withModule murl act
  where
    selectModule :: TargetGroup -> M ModuleUrl
    selectModule
      = promptSelectOne
      . mapMaybe f
      . toList

    f :: Hoogle.Item -> Maybe (ModuleUrl, P.Doc)
    f x = case x of
      Hoogle.Module m      -> Just (Hoogle.mUrl m, viewItemPackageAndModule x)
      Hoogle.Declaration d -> Just (Hoogle.dModuleUrl d, viewItemPackageAndModule x)
      Hoogle.Package _     -> Nothing

promptSelectOne :: [(a, P.Doc)] -> M a
promptSelectOne = \case
  []      -> throwError "No matching options"
  [(x,_)] -> return x
  xs      -> do
    liftIO $ putStrLn "Select one:"
    viewInTerminal $ P.vsep $ numbered $ map snd xs
    num <- getInputLine ": "
    case readMaybe =<< num of
      Just n -> case listToMaybe $ drop (n - 1) xs of
        Just (x, _) -> return x
        Nothing -> do
          liftIO $ hPutStrLn stderr "Invalid index"
          promptSelectOne xs
      Nothing -> do
        liftIO $ hPutStrLn stderr "Number not recognised"
        promptSelectOne xs

withModuleFromPackage :: String -> Package -> (Module -> M a) -> M a
withModuleFromPackage modName Package{..} act = do
  let url = packageModuleUrl pUrl modName
  html <- fetchHTML url
  let mod = parseModuleDocs url html
  State.modify' $ \s -> s{ sContext = ContextModule mod }
  act mod

withModuleFromPackageIx :: Int -> Package -> (Module -> M a) -> M a
withModuleFromPackageIx ix p act =
  withIx ix (pModules p) $ \m -> withModuleFromPackage m p act

withDeclFromModuleIx :: Int -> Module -> (Declaration -> M a) -> M a
withDeclFromModuleIx ix = withIx ix . mDeclarations

viewSearchResults :: [TargetGroup] -> M ()
viewSearchResults
  = viewInTerminal
  . P.vsep
  . reverse
  . numbered
  . map viewSummary

viewDeclaration :: Declaration -> M ()
viewDeclaration = viewInTerminalPaged . prettyDecl

viewDeclarationWithLink :: Declaration -> M ()
viewDeclarationWithLink decl = viewInTerminalPaged $ P.vcat
  [ prettyDecl decl
  , Haddock.link $ P.text $ getUrl (dDeclUrl decl)
  ]

viewModule :: View -> Module -> M ()
viewModule Interface = viewModuleInterface
viewModule Documentation = viewModuleDocs

viewModuleInterface :: Module -> M ()
viewModuleInterface mod =
  viewInTerminalPaged
  . P.vsep
  . (mainHeading (mTitle mod) :)
  . numbered
  . map (prettyHtml . dSignature)
  . mDeclarations
  $ mod

viewModuleDocs :: Module -> M ()
viewModuleDocs (Module name minfo decls murl) =
  viewInTerminalPaged $ P.vsep $
    [ mainHeading name
    , Haddock.link $  P.text $ getUrl murl
    ]
    ++
    [ prettyHtml info | Just info <- [minfo] ]
    ++
    [""]
    ++
    [ prettyDecl decl | decl <- decls ]


viewPackage :: View -> Package -> M ()
viewPackage Interface = viewPackageInterface
viewPackage Documentation = viewPackageDocs

viewPackageInterface :: Package -> M ()
viewPackageInterface Package{..} =
  viewInTerminalPaged $ P.vsep $
    mainHeading pTitle : numbered (P.text <$> pModules)

viewPackageDocs :: Package -> M ()
viewPackageDocs Package{..} = viewInTerminalPaged $ P.vsep $
  [ mainHeading $ case pSubTitle of
      Nothing -> pTitle
      Just s -> pTitle <> ": " <> s
  , Haddock.link $  P.text $ getUrl pUrl
  , section "Description" (prettyHtml pDescription)
  ]
  ++
  [ section "Readme" $ prettyHtml readme | Just readme <- [pReadme] ]
  ++
  [ section "Properties" (P.vsep $ map viewProp pProperties) ]
  where
    section heading body =
      P.text heading <> P.nest 2 (P.linebreak <> body)

    viewProp (title, body) =
      section title (prettyHtml body)


viewInTerminal :: P.Doc -> M ()
viewInTerminal doc = do
  noColours <- State.gets sNoColours
  printDoc noColours stdout doc

viewInTerminalPaged :: P.Doc -> M ()
viewInTerminalPaged doc = do
  noColours <- State.gets sNoColours
  withPager $ \handle -> printDoc noColours handle doc

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

printDoc :: MonadIO m => Bool -> Handle -> P.Doc -> m ()
printDoc noColours handle doc = liftIO $ do
  width <- min maxWidth . maybe maxWidth Terminal.width <$> Terminal.size
  P.displayIO handle $ P.renderSmart 1 width $
    if noColours
       then P.plain doc
       else doc
  hPutStrLn handle ""

viewSource :: DeclUrl -> M ()
viewSource durl = do
  url <- sourceLink durl
  html <- fetchHTML url
  viewInEditor (fileInfo url html)
  where
    viewInEditor :: FileInfo -> M ()
    viewInEditor (FileInfo filename mline content) = do
      let line = maybe "" (("+" <>) . show) mline
      liftIO $ do
        editor <- getEditor
        withSystemTempFile filename $ \fullpath handle -> do
          Text.hPutStr handle content
          hFlush handle
          Process.callCommand $ unwords [editor, fullpath, line]

getEditor :: IO String
getEditor = getEnv "EDITOR" <|> getEnv "VISUAL" <|> defaultEditor
  where
    defaultEditor = error "no editor selected"

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
  , P.indent 2 $ P.text str
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
  . map viewItemPackageAndModule
  . toList

viewPackageName :: String -> P.Doc
viewPackageName = P.magenta . P.text

viewModuleName :: String -> P.Doc
viewModuleName = P.black . P.text

viewItemPackage :: Hoogle.Item -> P.Doc
viewItemPackage = \case
  Hoogle.Declaration d -> viewPackageName (Hoogle.dPackage d)
  Hoogle.Module m      -> viewPackageName (Hoogle.mPackage m)
  Hoogle.Package p     -> viewPackageName (Hoogle.pTitle p)

viewItemPackageAndModule :: Hoogle.Item -> P.Doc
viewItemPackageAndModule item = case item of
  Hoogle.Declaration d -> viewItemPackage item P.<+> viewModuleName (Hoogle.dModule d)
  Hoogle.Module _      -> viewItemPackage item
  Hoogle.Package _     -> viewItemPackage item

prettyDecl :: Declaration -> P.Doc
prettyDecl Declaration{..} =
  P.vsep $ map prettyHtml (dSignatureExpanded:dContent)

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
      ] ++ reverse (Haddock.link . P.text . toUrl <$> toList tgroup)

-- ================================
-- Hoogle handling
-- ================================

-- | Get URL for source file for a target
sourceLink :: DeclUrl -> M SourceLink
sourceLink (DeclUrl murl anchor) = do
  html <- fetchHTML murl
  let links = sourceLinks murl html
  case lookup anchor links of
    Nothing -> throwError $ unlines $
      [ "anchor missing in module docs"
      , show murl
      ] ++ map show links
    Just slink -> return slink

declUrl :: Declaration -> DeclUrl
declUrl Declaration{..} =  DeclUrl dModuleUrl dAnchor

toPackageUrl :: ModuleUrl -> PackageUrl
toPackageUrl (ModuleUrl url) = PackageUrl $ fst $ breakOn "docs" url

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

-- =============================
--  HTTP requests
-- =============================

fetchHTML :: HasUrl a => a -> M HtmlPage
fetchHTML x = do
  req <- Http.parseRequest (getUrl x)
  src <- fetch req
  return (parseHtmlDocument src)

-- | Fetch and cache request's content
fetch :: Http.Request -> M LB.ByteString
fetch req = do
  cache <- State.gets sCache
  cached cache (show req) $ do
      liftIO $ hPutStrLn stderr $ "fetching: " <> uriToString id (Http.getUri req) ""
      manager <- State.gets sManager
      eitherRes <- liftIO $ try $ Http.httpLbs req manager
      res <- either (throwError . prettyHttpError) return eitherRes
      let status = Http.responseStatus res
      unless (Http.statusIsSuccessful status) $
        throwError
          $ "unable to fetch page: "
          <> Text.unpack (Text.decodeUtf8 $ Http.statusMessage status)
      return $ Http.responseBody res

  where
    prettyHttpError :: Http.HttpException -> String
    prettyHttpError httpErr = "*** HTTP Error: " <> case httpErr of
      Http.InvalidUrlException _ msg ->
         "invalid URL: " <> msg
      Http.HttpExceptionRequest _ err -> case err of
        Http.StatusCodeException res _ ->
          "invalid response status: " <> show (Http.responseStatus res)
        Http.TooManyRedirects _ -> "too many redirects"
        Http.OverlongHeaders -> "overlong headers"
        Http.ResponseTimeout -> "response timeout"
        Http.ConnectionTimeout -> "connection timeout"
        Http.ConnectionFailure _ ->
          "connection failure. Check your internet connection"
        Http.InvalidStatusLine _ -> "invalid status line"
        Http.InvalidHeader _ -> "invalid header"
        Http.InvalidRequestHeader _ -> "invalid request header"
        Http.InternalException e -> "internal exception: " <> show e
        Http.ProxyConnectException _ _ status ->
          "unable to connect to proxy: " <> show status
        Http.NoResponseDataReceived -> "no response data received"
        Http.TlsNotSupported -> "tls not supported"
        Http.WrongRequestBodyStreamSize _ _ -> "wrong request stream size"
        Http.ResponseBodyTooShort _ _ -> "reponse body too short"
        Http.InvalidChunkHeaders -> "invalid chunk headers"
        Http.IncompleteHeaders -> "incomplete headers"
        Http.InvalidDestinationHost _ -> "invalid destination host"
        Http.HttpZlibException e -> "zlib exception: " <> show e
        Http.InvalidProxyEnvironmentVariable var val ->
          "invalid proxy environment var: " <> show var <> ": " <> show val
        Http.ConnectionClosed -> "connection closed"
        Http.InvalidProxySettings _ -> "invalid proxy settings"
