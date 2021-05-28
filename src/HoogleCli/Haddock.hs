{-# LANGUAGE LambdaCase #-}
module HoogleCli.Haddock
  ( parseModuleDocs
  , prettyHTML
  , numbered
  , parseHoogleHtml
  , sourceLinks
  , fileInfo
  )
  where

-- | Functions to parse and display Haddock HTML

import HoogleCli.Types

import Data.List.Extra (unescapeHTML)
import Data.Foldable (fold)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List hiding (groupBy)
import Data.Char (isSpace)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as Text

import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.PrettyPrint.ANSI.Leijen.Internal as P

parseHoogleHtml :: String -> XML.Element
parseHoogleHtml
  = XML.documentRoot
  . HTML.parseLBS
  . LB.fromStrict
  . Text.encodeUtf8
  . Text.pack
  . (\v -> "<div>" <> v <> "</div>")

parseModuleDocs :: Url -> HTML -> ModuleDocs
parseModuleDocs url (HTML src) = head $ do
  let root = XML.documentRoot (HTML.parseLBS src)
  body    <- findM (is "body" . tag) $ children root
  content <- findM (is "content" . id_) $ children body
  let mtitle = do
        h <- findM (is "module-header" . id_) (children content)
        findM (is "caption" . class_) (children h)
      mdescription = findM (is "description" . id_) (children content)
  interface <- findM (is "interface" . id_) (children content)
  return ModuleDocs
    { mTitle = Text.unpack $ maybe "" innerText mtitle
    , mDescription = mdescription
    , mDeclarations = mapMaybe parseDeclaration $ children interface
    , mUrl = url
    }

parseDeclaration :: XML.Element -> Maybe DeclarationDocs
parseDeclaration el = do
  decl <- findM (is "top" . class_) [el]
  ([sig], content) <- return
    $ partition (is "src" . class_) $ children decl

  return DeclarationDocs
    { dAnchors = Set.fromList $ anchors el
    , dSignature = asTag "div" sig
    , dContent = content
    }
  where
    asTag t e = e
      { XML.elementName =
          (XML.elementName e) { XML.nameLocalName = t }
      }

findM :: (MonadFail m, Foldable t) => (a -> Bool) -> t a -> m a
findM f x = do
  Just a <- return $ find f x
  return a

is :: Eq a => a -> a -> Bool
is = (==)

children :: XML.Element -> [XML.Element]
children element =
  [ n | XML.NodeElement n <- XML.elementNodes element ]

tag :: XML.Element -> Text
tag = XML.nameLocalName . XML.elementName

id_ :: XML.Element -> Text
id_ = attr "id"

class_ :: XML.Element -> Text
class_ = attr "class"

attr :: Text -> XML.Element -> Text
attr name =
  fromMaybe ""
  . Map.lookup (XML.Name name Nothing Nothing)
  . XML.elementAttributes

innerText :: XML.Element -> Text
innerText el = flip foldMap (XML.elementNodes el) $ \case
  XML.NodeElement e -> innerText e
  XML.NodeInstruction _ -> mempty
  XML.NodeContent txt -> txt
  XML.NodeComment _ -> mempty

anchors :: XML.Element -> [Anchor]
anchors el = f $ foldMap anchors (children el)
  where
    f = if isAnchor el then (id_ el :) else id

    isAnchor e =
      class_ e == "def" &&
      (Text.isPrefixOf "t:" (id_ e) || Text.isPrefixOf "v:" (id_ e))

sourceLinks :: ModuleLink -> HTML -> [(Anchor, SourceLink)]
sourceLinks (ModuleLink modLink _) (HTML html) = do
  let root = XML.documentRoot (HTML.parseLBS html)
  body        <- filter (is "body" . tag) $ children root
  content     <- filter (is "content" . id_) $ children body
  interface   <- filter (is "interface" . id_) $ children content
  declaration <- filter (is "top" . class_) $ children interface

  signature  <- findM (is "src" . class_) $ children declaration
  url <- map (toSourceUrl . attr "href")
    . findM (is "Source" . innerText)
    $ children signature
  srcAnchor <- takeAnchor url
  let surl = SourceLink (dropAnchor url) srcAnchor

  let constructors = filter (is "subs constructors" . class_) $ children declaration
  anchor <- foldMap anchors (signature : constructors)
  return (anchor, surl)
  where
    parent = reverse . tail . dropWhile (/= '/') . reverse

    toSourceUrl relativeUrl = parent modLink <> "/" <> Text.unpack relativeUrl


-- ================================
-- Displaying Haddock's HTML
-- ================================

-- | Render Haddock's HTML
prettyHTML :: XML.Element -> P.Doc
prettyHTML = fromMaybe mempty . unXMLElement
  where
    unXMLElement e = style e . fold =<< unXMLChildren e
    unXMLChildren e =
      case mapMaybe unXMLNode (XML.elementNodes e) of
        [] -> Nothing
        xs -> Just xs
    unXMLNode = \case
      XML.NodeInstruction _ -> Nothing
      XML.NodeContent txt | Text.null txt -> Nothing
      XML.NodeContent txt -> Just
        $ docwords id
        $ unescapeHTML
        $ Text.unpack txt
      XML.NodeComment _ -> Nothing
      XML.NodeElement e -> unXMLElement e

    docwords f [] = P.fillCat (f [])
    docwords f (x:xs)
      | isSpace x = docwords (f . (P.space :)) $ dropWhile isSpace xs
    docwords f xs = docwords (f . (P.text w :)) ys
      where (w, ys) = break isSpace xs

    style e m = classStyle e m  >>= tagStyle e

    classStyle e = case class_ e of
      ""                  -> Just
      -- layout
      "doc"               -> Just . P.nest 2
      "subs methods"      -> Just . P.nest 2
      "subs instances"    -> Just . P.nest 2
      "subs constructors" -> Just . P.nest 2
      -- a declaration wrapper
      "top"               -> const
                              $ Just . mappend P.hardline . P.vsep
                              $ mapMaybe unXMLElement (children e)
      -- style
      "caption"           -> Just . P.bold
      "name"              -> Just . P.dullgreen
      "def"               -> Just . P.bold
      "fixity"            -> Just . italics . P.black
      -- invisible
      "link"              -> hide
      "selflink"          -> hide
      -- modify
      "module-header"     -> const $ unXMLElement =<< findM (is "caption" . class_) (children e)
      _                   -> Just

    tagStyle e = case tag e of
       "h1"      -> Just . linebreak . mappend (P.text "# ")
       "h2"      -> Just . linebreak . mappend (P.text "## ")
       "h3"      -> Just . linebreak . mappend (P.text "### ")
       "h4"      -> Just . linebreak . mappend (P.text "#### ")
       "h5"      -> Just . linebreak . mappend (P.text "##### ")
       "h6"      -> Just . linebreak . mappend (P.text "###### ")
       "tt"      -> Just . P.green
       "pre"     -> const
                      $ Just . P.nest 2 . P.black . linebreak . P.string . Text.unpack
                      $ innerText e
       "code"    -> Just . P.black
       "a"       -> Just . P.cyan
       "b"       -> Just . P.bold
       "p"       -> Just . linebreak
       "dt"      -> Just . P.bold . linebreak
       "dd"      -> Just . linebreak
       "summary" -> Just . linebreak
       "ol"      -> const $ Just . linebreak . P.vsep . numbered $ mapMaybe unXMLElement (children e)
       "ul"      -> const $ Just . linebreak . P.vsep . map bullet $ mapMaybe unXMLElement (children e)
       "td"      | isInstanceDetails e -> hide
                 | otherwise -> Just
       "table"   -> const
                      $ Just .  flip mappend P.hardline . P.vsep . map bullet
                      $ mapMaybe unXMLElement (children e)
       -- don't show instance details
       _         -> Just

    isInstanceDetails e = tag e == "td" && attr "colspan" e == "2"
    linebreak doc = P.hardline <> doc <> P.hardline
    italics = P.Italicize True
    hide = const Nothing

-- | Convert an html page into a src file and inform of line
-- number of SourceLink
fileInfo :: SourceLink -> HTML -> FileInfo
fileInfo (SourceLink _ anchor) (HTML doc) = head $ do
  let page = HTML.parseLBS doc
      root = XML.documentRoot page
  head_ <- filter (is "head" . tag) $ children root
  title <- filter (is "title" . tag) $ children head_
  let filename = Text.unpack $ Text.replace "/" "." $ innerText title <> ".hs"
  body <- filter (is "body" . tag) $ children root
  return $ FileInfo filename (anchorLine anchor body) (innerText body)

-- | File line where the tag is
anchorLine :: Anchor -> XML.Element -> Maybe Int
anchorLine anchor
  = either Just (const Nothing)
  . anchorNodes 0
  . XML.elementNodes
  where
    anchorNodes :: Int -> [XML.Node] -> Either Int Int
    anchorNodes n = foldM anchorNode n

    anchorNode :: Int -> XML.Node -> Either Int Int -- anchor line or total lines
    anchorNode n = \case
      XML.NodeInstruction _ -> Right n
      XML.NodeContent txt -> Right $ n + Text.count "\n" txt
      XML.NodeComment _ -> Right n
      XML.NodeElement e ->
        if attr "name" e == anchor
          then Left n
          else anchorNodes n (XML.elementNodes e)


-- =================================
-- Pretty priting
-- =================================

numbered :: [P.Doc] -> [P.Doc]
numbered = zipWith f [1..]
  where
    f n s = P.fill 2 (P.blue $ P.int n) P.<+> P.align s

bullet :: P.Doc -> P.Doc
bullet doc = P.fill 2 (P.char '-') <> P.align doc

