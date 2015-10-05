{-# LANGUAGE OverloadedStrings #-}

module Site.Conf where

import Hakyll

myDestinationDirectory :: String
myDestinationDirectory = "build"

myStoreDirectory :: String
myStoreDirectory = ".cache"

myTmpDirectory :: String
myTmpDirectory = ".cache/tmp"

myDeployCommand :: String
myDeployCommand = "ghp-import -p build"

myPreviewDirectory :: String
myPreviewDirectory = "preview"

myConf :: Configuration
myConf = defaultConfiguration {
  destinationDirectory = myDestinationDirectory,
  storeDirectory       = myStoreDirectory,
  tmpDirectory         = myTmpDirectory,
  deployCommand        = myDeployCommand
  }
