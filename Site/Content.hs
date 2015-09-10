{-# LANGUAGE OverloadedStrings #-}

module Site.Content (
  contentRules
  ) where

import Hakyll

import Data.Monoid (mconcat, (<>))
import Data.List
import Data.Char
import Site.Contexts
import System.FilePath

baseRoute :: Routes
baseRoute = gsubRoute "pages/|static/" (const "")

allPosts :: Pattern
allPosts = "posts/**.md"

canonName :: String -> String
canonName = intercalate "-" . words . map (\x -> if x `elem` allowedChars
                                                 then toLower x
                                                 else ' ')
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

contentRules :: Rules ()
contentRules = do
  tags <- buildTags allPosts (fromCapture "tags/*" . canonName)

  -- Copy everything as a "raw" version
  match ("posts/**" .||. "pages/**" .||. "static/**") $
    version "raw" $ do
      route   baseRoute
      compile copyFileCompiler

  -- Compile all markdown
  match allPosts $
    compileContents "post" (postCtx tags) pandocCompiler
  match "pages/**.md" $
    compileContents "page" (postCtx tags) pandocCompiler

  -- Create archive
  create ["tags"] $
    compileContents "tags" (archiveCtx allPosts tags Nothing) (makeItem "")

  tagsRules tags $ \tag pattern ->
    compileContents "tags" (archiveCtx pattern tags $ Just tag) (makeItem "")


compileContents :: String -> Context String -> Compiler (Item String) -> Rules()
compileContents tpl ctx compiler = do
  let indexRoute = customRoute $ \ident ->
        (dropExtension $ toFilePath ident) ++ "/index.html"

  route   $ baseRoute `composeRoutes` indexRoute
  compile $ compiler
    >>= loadAndApplyTemplate template ctx
    >>= loadAndApplyTemplate "templates/layout.html" ctx
    >>= relativizeUrls
    where template = fromFilePath $ "templates/" ++ tpl ++ ".html"
