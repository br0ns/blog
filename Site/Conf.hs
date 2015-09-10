{-# LANGUAGE OverloadedStrings #-}

module Site.Conf where

import Hakyll

import System.FilePath

myDestinationDirectory = "build"
myStoreDirectory = ".cache"
myTmpDirectory = ".cache/tmp"
myDeployCommand = "ghp-import -p build"
myPreviewDirectory = "preview"

myConf = defaultConfiguration {
  destinationDirectory = myDestinationDirectory,
  storeDirectory       = myStoreDirectory,
  tmpDirectory         = myTmpDirectory,
  deployCommand        = myDeployCommand
  }
