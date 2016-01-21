{-# LANGUAGE OverloadedStrings #-}

module Site.JS (
  jsCompiler
  ) where

import Hakyll

jsCompiler :: Compiler (Item String)
jsCompiler = do
  -- p <- getResourceFilePath
  -- body <- unixFilter "closure-compiler" [p] ""
  -- makeItem body
  getResourceBody
