{-# LANGUAGE OverloadedStrings #-}

module Site.SCSS (
  scssRule
  ) where

import Hakyll

scssCompiler :: String -> Compiler (Item String)
scssCompiler p = do
  body <- loadBody $ fromFilePath p
  body' <- unixFilter "sass" args body
  makeItem body'
    where args = [ "--stdin"
                 , "--scss"
                 , "--style", "compressed"
                 , "--compass"
                 , "--load-path", "css/" ]

scssRule :: Rules ()
scssRule = do
  let scss = "css/*.scss"

  -- Add all SCSS files to the cache
  match scss $ compile getResourceString

  -- Make this rule dependent on them all
  scssDeps <- makePatternDependency scss
  rulesExtraDependencies [scssDeps] $
    create ["css/style.css"] $ do
      route   idRoute
      compile $ scssCompiler "css/style.scss"
