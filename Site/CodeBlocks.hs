{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.CodeBlocks (
  pygmentsServer,
  codeBlocks
) where

import Site.Types
import Site.Conf
import Site.FontAwesome

import Hakyll

import Text.Pandoc
import Text.Pandoc.Walk (walkM)

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.State

import System.Directory
import System.FilePath
import System.Environment

import Data.Maybe
import Data.List
import Data.String

import qualified System.IO.Streams as S
import System.IO.Streams.Process (runInteractiveProcess)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString as BS

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pygmentsServer :: IO Streams
pygmentsServer = do
  (inp, out, _, _) <- runInteractiveProcess "python" ["Site/pygments-server.py"] Nothing Nothing
  return (inp, out)

codeBlocks :: Streams -> Pandoc -> Compiler Pandoc
codeBlocks streams doc = do
  (doc', cbs) <- runStateT (walkM (generateCodeBlock streams) doc) []
  unsafeCompiler $ forM_ cbs $ \(path, CodeBlock _ contents) -> do
    -- Nasty hack.  Why isn't there a Compiler to access the underlying
    -- configuration?
    action:_ <- getArgs
    let dstDir = if action == "watch"
                 then myPreviewDirectory
                 else myDestinationDirectory
    putStrLn $ "Wrinting " ++ path
    let path' = dstDir </> path
    createDirectoryIfMissing True $ takeDirectory path'
    writeFile path' $ contents ++ "\n"

  return doc'

-- TODO: add extension from `pygments.lexers._mappings`
extensions :: [(String, String)]
extensions = [
  ("python", "py"),
  ("haskell", "hs"),
  ("c"     , "c")
  ]

-- While we walk the document we also collect the code blocks so we can write
-- them to disk for easy download; this is the purpose of the `StateT`
generateCodeBlock :: Streams -> Block -> StateT [(FilePath, Block)] Compiler Block
generateCodeBlock streams block@(CodeBlock (ident, classes, keyvals) contents) = do
  let lang = fromMaybe (if null classes then "text" else head classes) $ lookup "lang" keyvals
      doDownload = "no-download" `notElem` classes &&
                   (maybe True (/= "no") $ lookup "download" keyvals)
      kvs = filter (flip elem ["hl_lines", "linenostart"] . fst) keyvals

  code <- lift $ pygmentize streams lang kvs contents

  blockUrl <- if doDownload then do
    dir <- lift $ (return.takeDirectory.fromJust) =<< getRoute =<< getUnderlying
    n <- gets length
    let ext = maybe "" (\e -> "." ++ e) $ lookup lang extensions
        ident' = if null ident
                 then "snippet" ++ show n ++ ext
                 else if hasExtension ident
                      then ident
                      else addExtension ident ext
        blockPath = dir </> ident'
    modify $ (:) (blockPath, block)
    return $ toUrl blockPath
               else return ""

  let colored = code
      caption = maybe ""
                (renderHtml . H.figcaption . H.span . preEscapedToHtml) $
                lookup "caption" keyvals
      download = renderHtml
                 $ H.div ! A.class_ "buttons-container"
                 $ if doDownload then
                     H.a ! (A.href (H.toValue blockUrl) <>
                            A.class_ "download-link" <>
                            A.title "Download snippet"
                           )
                     $ fontAwesome "download"
                   else
                     ""
      composed = renderHtml
                 $ H.div ! A.class_ "codeblock-container"
                 $ H.figure ! A.class_ classes'
                 $ preEscapedToHtml
                 $ download ++ colored ++ caption
      classes' = fromString (unwords $ "codeblock" : classes)

  return $ RawBlock "html" composed
generateCodeBlock _ x = return x

pygmentize :: Streams -> String -> [(String, String)]-> String ->
              Compiler String
pygmentize (os, is) lang keyvals contents = unsafeCompiler $ do
  let lang'     = U8.fromString lang
      keyvals'  = U8.fromString $ intercalate "\0"
                  $ map (\(k, v) -> k ++ "=" ++ v) keyvals
      contents' = U8.fromString contents
      len       = U8.fromString . show . BS.length $ contents'

      -- REQUEST:  LANG\nLENGTH\nCODE
      request = C.intercalate "\n" [lang', keyvals', len, contents']

  mapM_ (flip S.write os . Just) [request, ""]

  -- RESPONSE: LENGTH\nRESPONSE
  responseLength <- read . U8.toString . fromJust <$> (S.lines >=> S.read) is
  U8.toString <$> S.readExactly responseLength is
