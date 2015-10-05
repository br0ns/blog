{-# LANGUAGE OverloadedStrings #-}

module Site.FontAwesome (fontAwesome) where

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

fontAwesome :: String -> H.Html
fontAwesome icon =
  H.i ! A.class_ (H.toValue $ "fa fa-" ++ icon) $ ""
