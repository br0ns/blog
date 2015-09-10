{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid ((<>), mappend)
import GHC.IO.Encoding
import System.FilePath
import Data.Monoid (mconcat, (<>))
import Data.List
import Data.Char
import Site.Contexts
import System.FilePath
import System.Environment
import Control.Monad.Trans
import Control.Monad
import Control.Concurrent

import Hakyll

import Site.Conf
import Site.SCSS
import Site.JS
import Site.Contexts
import Site.Pandoc
import Site.LiveEdit
import Site.CodeBlocks (pygmentsServer)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  action : _ <- getArgs
  streams <- pygmentsServer
  channels <- newChannels

  let watch = action == "watch"
      clean = action == "clean"
      conf =
        if watch
        then myConf {
          destinationDirectory = myPreviewDirectory,
          storeDirectory = myPreviewDirectory </> "cache",
          tmpDirectory = myPreviewDirectory </> "cache" </> "tmp"
          }
        else myConf
      compileContents tpl ctx compiler = do
        let indexRoute = customRoute $ \ident ->
              (dropExtension $ toFilePath ident) ++ "/index.html"
            ctx' = ctx <> liveEditJSField "live_edit_js" watch

        route   $ baseRoute `composeRoutes` indexRoute
        compile $ compiler
          >>= webSocketPipe channels
          >>= loadAndApplyTemplate template ctx'
          >>= loadAndApplyTemplate "templates/layout.html" ctx'
          >>= absolutizeUrls
          >>= saveSnapshot "absolute_urls"
          >>= relativizeUrls
        where template = fromFilePath $ "templates/" ++ tpl ++ ".html"

  when watch $ void . forkIO $ webSocketServer channels
  when clean $ do
    putStrLn $ "Removing " ++ myPreviewDirectory ++ "..."
    removeDirectory myPreviewDirectory

  hakyllWith conf $ do

    let postsPattern = if watch
                       then "posts/**.md" .||. "drafts/**.md"
                       else "posts/**.md"

    -- Style sheets
    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    -- Compile css/style.scss
    scssRule

    -- Javascript
    match "js/*" $ do
      route   idRoute
      compile $ if watch
                then getResourceBody
                else jsCompiler

    -- Compile templates
    match "templates/*" $ compile templateCompiler

    -- The actual contents
    tags <- buildTags postsPattern (fromCapture "tags/*" . canonName)

    -- Copy everything as a "raw" version
    match ("posts/**" .||. "pages/**" .||. "static/**") $
      version "raw" $ do
        route   baseRoute
        compile copyFileCompiler

    let pandocCompiler = pandocCustomCompiler streams

    -- Compile all markdown
    match postsPattern $
      compileContents "post" (postCtx tags) pandocCompiler
    match "pages/**.md" $
      compileContents "page" defaultCtx pandocCompiler

    -- Create archive
    create ["tags/all"] $
      compileContents "tags" (archiveCtx postsPattern tags Nothing) (makeItem "")

    tagsRules tags $ \tag pattern ->
      compileContents "tags" (archiveCtx pattern tags $ Just tag) (makeItem "")

    -- 404 page
    create ["404.html"] $
      compileContents "404"
      (constField "page_title" "Not found - br0ns" <>
       constField "title" "404" <>
       constField "byline" "Not found" <>
       constField "url" "/" <>
       bodyField  "body"
      ) (makeItem "")

    -- Put the latest post on the front page
    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentGitFirst postsPattern
        let post = head posts
        (body :: String) <- loadSnapshotBody (itemIdentifier post) "absolute_urls"
        relativizeUrls =<< makeItem body

baseRoute :: Routes
baseRoute = gsubRoute "pages/|static/" (const "")

canonName :: String -> String
canonName = intercalate "-" . words . map (\x -> if x `elem` allowedChars
                                                 then toLower x
                                                 else ' ')
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

-- compileContents :: String -> Context String -> Compiler (Item String) -> Rules()

absolutizeUrls :: Item String -> Compiler (Item String)
absolutizeUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (withUrls abs) item
          where abs url = if isAbs url
                          then url
                          else "/" ++ takeDirectory r </> url
                isAbs url = "://" `isInfixOf` url || "/" `isPrefixOf` url
