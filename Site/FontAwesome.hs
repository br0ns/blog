module Site.FontAwesome (fontAwesome) where

import Text.Blaze.Html ((!), toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

fontAwesome :: String -> H.Html
fontAwesome icon =
  H.i ! A.class_ (H.toValue $ "fa fa-" ++ icon) $ toHtml ""
