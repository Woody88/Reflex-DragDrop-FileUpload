{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Dev (
    run
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Reflex.Dom.Core

import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (ConnectionOptions, defaultConnectionOptions)

import           Language.Javascript.JSaddle            (JSM)
import           Language.Javascript.JSaddle.Run        (syncPoint)
import Language.Javascript.JSaddle.Run.Files (indexHtml)
import           Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleOr, jsaddleApp, jsaddleJs)
import qualified Network.Wai                            as W
import           Network.Wai.Middleware.Static

import           System.FilePath                        ((</>))
import           System.Directory                       (listDirectory)
import qualified Data.Text                              as Text
import qualified Data.Map                               as Map
import Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Types as H (status403, status200)



indexHtml' :: ByteString
indexHtml' =
    "<!DOCTYPE html>\n\
    \<html class=\"uk-height-1-1\">\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body class=\"uk-height-1-1\">\n\
    \</body>\n\
    \<script src=\"jsaddle.js\"></script>\n\
    \</html>"

jsaddleWithAppOr :: ConnectionOptions -> JSM () -> W.Application -> IO W.Application
jsaddleWithAppOr opts entryPoint otherApp = jsaddleOr opts entryPoint $ \req sendResponse ->
  (fromMaybe (otherApp req sendResponse)
  (jsaddleAppPartialWithJs (jsaddleJs True) req sendResponse))

jsaddleAppPartialWithJs :: ByteString -> W.Request -> (W.Response -> IO W.ResponseReceived) -> Maybe (IO W.ResponseReceived)
jsaddleAppPartialWithJs js req sendResponse =
  case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml'
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] js
    _ -> Nothing

-- | A @main@ for doing development.
devMain :: W.Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend

  runSettings (defaultSettings & setTimeout 3600 & setPort port) app

-- | A version of @devMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devMainAutoReload :: W.Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port

headSection ::
  MonadWidget t m =>
  FilePath ->
    FilePath ->
  [FilePath] ->
  [FilePath] ->
  m ()
headSection jsPath cssPath cssFiles jsFiles =
  let
    stylesheet s =
      elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
        pure ()
    script s =
      elAttr "script" (Map.fromList [ ("type", "text/javascript"),("src", s)]) $
        pure ()
  in do
    elAttr "meta" ("charset" =: "utf-8") $ blank
    
    -- stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    -- stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
    traverse_ (\f -> stylesheet . Text.pack $ cssPath </> f) cssFiles
    traverse_ (\f -> script . Text.pack $ jsPath </> f) jsFiles

serveCss :: FilePath -> FilePath -> (forall x. Widget x ()) -> IO (W.Application, JSM ())
serveCss jsPath cssPath w = do
  jsFiles <- listDirectory $ "." </> jsPath
  cssFiles <- listDirectory $ "." </> cssPath
  let
    frontendApp =
      mainWidgetWithHead (headSection jsPath cssPath cssFiles jsFiles) w
    backendApp  =
      staticPolicy $ hasPrefix "assets"  
  pure (backendApp jsaddleApp, frontendApp)

run' ::
  FilePath ->
  FilePath ->
  Int ->
  (forall x. Widget x ()) ->
  IO ()
run' jsPath cssPath port w = do
  (backendApp, frontendApp) <- serveCss jsPath cssPath w
  devMainAutoReload backendApp frontendApp port

run ::
  (forall x. Widget x ())
  -> IO ()
run =
  run' "assets/js" "assets/css" 3000
