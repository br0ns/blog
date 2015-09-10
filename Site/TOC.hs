{-# LANGUAGE OverloadedStrings #-}

module Site.TOC (
  tableOfContents,
) where

import Site.FontAwesome

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)

import Data.List (groupBy)
import Data.Tree (Forest, Tree(Node))
import Data.Monoid ((<>), mconcat)
import Data.Function (on)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad

import Text.Blaze.Html (preEscapedToHtml, (!), toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

collectHeader :: Block -> Maybe Block
collectHeader header@(Header level (_, classes, _) _) =
  if "unnumbered" `elem` classes || "notoc" `elem` classes || level > 3
  then Nothing
  else Just header
collectHeader _ = Nothing

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

markupTocHeader :: Tree Block -> H.Html
markupTocHeader (Node (Header _ (ident, _, keyvals) inline) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupTocHeaders headers)
  where render x  = writeHtmlString def (Pandoc nullMeta [(Plain x)])
        section   = fromMaybe (render inline) (lookup "toc" keyvals)
        link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml section
markupTocHeader _ = error "what"

markupTocHeaders :: Forest Block -> H.Html
markupTocHeaders = mconcat . map markupTocHeader

markupHeader :: Block -> Block
markupHeader header@(Header level (ident, classes, keyvals) inline) =
  case collectHeader header of
    Nothing -> Header level (ident, "notoc" : classes, keyvals) inline
    Just _  -> Header level ("", classes, keyvals) $ [
      RawInline "html" $ renderHtml $ leftof <> anchor <> link
      ]
  where render x  = writeHtmlString def (Pandoc nullMeta [(Plain x)])
        section   = render inline
        link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml section
        icon      = H.span ! A.class_ "link-icon" $ fontAwesome "link"
        number    = H.span ! A.class_ "section-number" $ ""
        anchor    = H.span ! (A.class_ "anchor" <> A.id (H.toValue ident)) $ ""
        leftof    = H.span ! A.class_ "left-of" $ icon <> number

markupHeader x = x

createTable :: Forest Block -> H.Html
createTable headers =
  (H.nav ! A.id "toc") $ do
    H.h3 "Contents"
    H.div ! A.class_ "hr" $ ""
    H.ol $ markupTocHeaders headers

generateTOC :: [Block] -> Block -> Block
generateTOC [] x = x
generateTOC headers x@(BulletList (( (( Plain ((Str "toc"):_)):_)):_)) =
  render . table $ headers
  where render = (RawBlock "html") . renderHtml
        table  = createTable . groupByHierarchy
generateTOC _ x = x

tableOfContents :: Pandoc -> Pandoc
tableOfContents doc =
  let headers = query (maybeToList . collectHeader) doc
  in walk (generateTOC headers) $ walk markupHeader $ doc
