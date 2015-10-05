{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Site.Contexts (
  defaultCtx,
  postCtx,
  archiveCtx,
  liveEditJSField,
  recentGitFirst,
  ) where

import Data.Monoid (mconcat, mempty, (<>))
import Data.Maybe
import Control.Applicative (empty, (<$>))
import Control.Monad
import System.FilePath
import System.Process
import Data.List
import Data.Ord
import Data.List.Split

import           Data.Time.LocalTime           (LocalTime (..))
import qualified Data.Time.Format              as TF
import           Data.Time.Locale.Compat       (defaultTimeLocale)

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import Hakyll

defaultCtx :: Context String
defaultCtx = mconcat [
  bodyField         "body",
  pageTitleField    "page_title",
  noIndexUrlField   "url",
  pathField         "path",

  metadataField
  ]

contentCtx :: Context String
contentCtx = mconcat [
  gitField          "git_hash"      "%H",
  gitField          "git_abbr_hash" "%h",
  gitField          "git_subject"   "%s",

  gitPublishedField "date" "%B %e, %Y",
  gitPublishedField "date_archive" "%F",
  gitEditedField    "date_edited" "%F %T",

  -- Fallback values if file is not committed to git
  constField        "date"          fallback_date,
  constField        "date_archive"  fallback_date,
  constField        "git_hash"      fallback_git_hash,
  constField        "git_abbr_hash" $ take 7 fallback_git_hash,
  constField        "git_subject"   "Not yet committed to Git",

  defaultCtx
  ]
  where fallback_date = "<span style=\"color:red;\">1970-01-01</span>"
        fallback_git_hash = "0123456789012345678901234567890123456789"


postCtx :: Tags -> Context String
postCtx tags = mconcat [
  tagsNoIndexField  "tags" tags,
  contentCtx
  ]

data TagDesc = TagDesc {
  tagName :: String,
  tagUrl  :: String,
  tagRefs :: Int
  }

recentGitFirst :: Pattern -> Compiler [Item String]
recentGitFirst pat =  do
  posts <- loadAll (pat .&&. hasNoVersion)
  liftM reverse
    $ sortOnM key posts
    where
      key i = do
        ts <- gitLog ["--date=raw"] "%ad" i
        return $ case ts of
          [] -> "z" -- slight hack here: z orders higher than all numbers
          _  -> last ts
      -- Borrowed from Hakyll
      sortOnM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
      sortOnM f xs = liftM (map fst . sortBy (comparing snd)) $
                     mapM (\x -> liftM (x,) (f x)) xs

archiveCtx :: Pattern -> Tags -> Maybe String -> Context String
archiveCtx pat tags mbtag = mconcat [
  listField "posts" contentCtx $ recentGitFirst pat,

  listField "tags_list" tagCtx
  $ sortOn (negate.tagRefs)
  $ mapM buildTagDesc
  $ addAllTag
  $ tagsMap tags,

  constField "title" "Archive",
  constField "byline" (maybe empty ("Tagged: " ++) mbtag),

  defaultCtx
  ] <> (case mbtag of
           Just tag -> constField "tag" tag
           Nothing  -> mempty
       )
  where tagCtx = mconcat [
          field "tag" $ return.tagName.itemBody,
          field "refs" $ return.show.tagRefs.itemBody,
          field "url" $ return.tagUrl.itemBody
          ]
        buildTagDesc (tag, ids) = do
          makeItem $
            TagDesc {
              tagName = tag,
              tagUrl  = (case tag of
                            -- "All" -> "tags"
                            _     -> toUrl $ toFilePath $ tagsMakeId tags tag
                        ),
              tagRefs = length ids
              }
        sortOn f =
          fmap (sortBy . comparing $ f . itemBody)
        addAllTag tagMap =
          ("All", nub $ concat $ fmap snd tagMap) : tagMap

liveEditJSField :: String -> Bool -> Context String
liveEditJSField key watch = field key $ \item ->
  if watch
  then do
    path <- toFilePath <$> getUnderlying
    let ctx = constField "path" path <> constField "port" "8001"

    tpl <- loadBody "templates/live-edit-js.html"
    itemBody <$> applyTemplate tpl ctx item
  else empty

-- Taken directly from Hakyll
tagsNoIndexField :: String -> Tags -> Context a
tagsNoIndexField =
  tagsFieldWith getTags renderLink (mconcat . intersperse ", ")
  where renderLink _   Nothing         = Nothing
        renderLink tag (Just filePath) =
          Just $ H.a ! A.href (toValue $ toUrl $ stripIndex $ filePath) $ toHtml tag
        stripIndex url =
          if takeFileName url == "index.html"
          then takeDirectory url
          else url

pageTitleField :: String -> Context String
pageTitleField k = field k $ \item -> do
  maybe "br0ns" (++ "- br0ns") <$>
    getMetadataField (itemIdentifier item) "title"

noIndexUrlField :: String -> Context String
noIndexUrlField k = field k noIndexUrl

noIndexUrl :: Item a -> Compiler String
noIndexUrl =
  fmap (maybe empty $ toUrl . stripIndex) . getRoute . itemIdentifier
    where stripIndex url =
            if takeFileName url == "index.html"
            then takeDirectory url
            else url

gitLog :: Show a => [String] -> String -> Item a -> Compiler [String]
gitLog extraArgs format item = do
  let p = toFilePath $ itemIdentifier item
      args = [ "log",
               "--format=" ++ format
             ] ++ extraArgs ++ [ "--", p ]

  res <- unsafeCompiler $ readProcess "git" args ""
  return $ init $ splitOn "\n" res

nonNull :: [a] -> Compiler [a]
nonNull = \case
  [] -> empty
  xs -> return xs

gitField :: String -> String -> Context String
gitField key format = field key $ \item ->
  head <$> (nonNull =<< gitLog [] format item)

gitLogDate :: Show a => String -> Item a -> Compiler [String]
gitLogDate format item = do
  dates <- gitLog ["--date=iso"] "%ad" item
  return $ catMaybes $ fmap convert dates
  where convert = fmap (formatTime format) . parseTime "%F %T %z"

parseTime :: String -> String -> Maybe LocalTime
parseTime = TF.parseTime defaultTimeLocale

formatTime :: String -> LocalTime -> String
formatTime = TF.formatTime defaultTimeLocale

gitPublishedField :: String -> String -> Context String
gitPublishedField key format = field key $ \item ->
  last <$> (nonNull =<< gitLogDate format item)

gitEditedField :: String -> String -> Context String
gitEditedField key format = field key $ \item -> do
  dates <- gitLogDate format item
  if length dates > 1
    then return $ head dates
    else empty
