{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Data.Monoid ((<>), mempty)
import GHC.IO.Encoding
import System.FilePath
import Data.List
import Data.Char
import System.Environment
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
              let path = toFilePath ident
              in if takeExtension path == ".html"
                 then path
                 else (++ "/index.html") $ dropExtension path
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
        pagesPattern = "pages/**.md" .&&. complement "pages/404.md"

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

    let contentCompiler = pandocCustomCompiler streams

    -- Compile all markdown

    match postsPattern $
      compileContents "post" (postCtx tags) contentCompiler
    match pagesPattern $
      compileContents "page" defaultCtx contentCompiler

    -- Create archive
    create ["tags/all"] $
      compileContents "tags" (archiveCtx postsPattern tags Nothing) (makeItem "")

    tagsRules tags $ \tag pattern ->
      compileContents "tags" (archiveCtx pattern tags $ Just tag) (makeItem "")

    -- 404 page
    match "pages/404.md" $ do
      compileContents "page" defaultCtx contentCompiler
      -- Override route; created below
      route mempty

    -- The 404 page may be accessed from any path, so we need absolute urls
    create ["404.html"] $ do
      route idRoute
      compile $
        (makeItem =<< loadSnapshotBody "pages/404.md" "absolute_urls" ::
            Compiler (Item String))

    -- Put the latest post on the front page
    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentGitFirst postsPattern
        let post = head posts
            ident = itemIdentifier post
        (body :: String) <- loadSnapshotBody ident "absolute_urls"
        relativizeUrls =<< makeItem body

baseRoute :: Routes
baseRoute = gsubRoute "pages/|static/" (const "")

canonName :: String -> String
canonName = intercalate "-" . words . map (\x -> if x `elem` allowedChars
                                                 then toLower x
                                                 else ' ')
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

absolutizeUrls :: Item String -> Compiler (Item String)
absolutizeUrls item = do
    mbroute <- getRoute $ itemIdentifier item
    return $ case mbroute of
        Nothing -> item
        Just r  -> fmap (withUrls absolutize) item
          where absolutize url = if isAbs url
                          then url
                          else "/" ++ takeDirectory r </> url
                isAbs url = "://" `isInfixOf` url || "/" `isPrefixOf` url
