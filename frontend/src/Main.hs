{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

-- import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.Encoding as T
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Map as M
-- import Data.ByteString as B
import Data.Maybe (fromMaybe, listToMaybe)
-- import Data.JSString.Text as T
-- import qualified Data.Aeson as AE
import qualified GHCJS.DOM.DataTransfer         as DOM
import qualified GHCJS.DOM.FileReader           as DOM
import qualified GHCJS.DOM.FileList             as DOM
import qualified GHCJS.DOM.File                 as DOM
import qualified GHCJS.DOM.Element              as DOM
import qualified GHCJS.DOM.EventM               as DOM
import qualified GHCJS.DOM.MouseEvent           as DOM
import qualified GHCJS.DOM.Types                as DOM 
import qualified Foreign.JavaScript.Utils       as DOM
-- import Language.Javascript.JSaddle.Warp (run)
-- import Dev
-- import Reflex.Dom.Core (mainWidget, mainWidgetWithHead)
-- import Reflex.Dom hiding (run, mainWidget, mainWidgetWithHead)
import Reflex.Dom 
import Data.Monoid ((<>))

data Action = Drop' | Dragover' | FileContent Text | NoFile

-- main :: IO ()
-- main = run mainEntry

main :: IO ()
main = mainWidget mainEntry

-- headWidget :: MonadWidget t m => m ()
-- headWidget = do
--     elAttr "meta"   ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
--     elAttr "link"   ("href" =: "assets/css/uikit.min.css" <> "rel" =: "stylesheet")           blank
--     elAttr "link"   ("href" =: "assets/css/style.css" <> "rel" =: "stylesheet")               blank
--     elAttr "script" ("src" =: "assets/js/uikit.min.js" <> "type" =: "text/javascript")        blank
--     elAttr "script" ("src" =: "assets/js/uikit-icons.min.js"  <> "type" =: "text/javascript") blank

mainEntry :: MonadWidget t m => m ()
mainEntry = 
    elAttr "div" ("class" =: "uk-flex uk-flex-column uk-flex-center uk-flex-middle uk-width-1-1") mainEntry'

mainEntry' :: MonadWidget t m => m ()
mainEntry' = mdo
    dropEl@(_, fileDyn) <- dropWidget $ leftmost [Dragover' <$ dragOverEv, Drop' <$ dropEv, fmap FileContent fileText, NoFile <$ never]
    mkFile (fmap FileContent fileText)
    -- fileDownloaderWidget
    -- dragEl <- elAttr' "div" ("style" =: "width: 250px; height: 250px; background-color: green; margin-bottom: 5px;" <> "draggable" =: "true") $ blank
    -- dragStartEv <- dragStartHandler dragEl
    let selectFileEv = listToMaybe <$> (updated fileDyn)
    dragOverEv   <- dragOverHandler dropEl 
    dropEv       <- dropHandler dropEl
    fileText     <- holdDropEvent (leftmost [selectFileEv, dropEv]) readDropFile
    pure ()

mkFile :: MonadWidget t m => Event t Action -> m ()
mkFile ev = do
    dEv <- holdDropEvent ev mkFile'
    dDownloader <- holdDyn Nothing dEv
    let dynAttr = fmap parseAction dDownloader
    elDynAttr "a" dynAttr $ text "Download"
    pure ()
    where parseAction :: Maybe Text -> M.Map Text Text
          parseAction (Just t) = ("class" =: "uk-card uk-card-default uk-width-1-1 uk-card-hover" <> "id" =: "file-download" <> "href" =: t <> "download" =: "file.txt")
          parseAction Nothing =  ("class" =: "uk-card uk-card-default uk-width-1-1 uk-card-hover" <> "id" =: "file-download" <> "hidden" =: "")

mkFile' :: MonadWidget t m => Action -> m (Event t (Maybe Text))
mkFile' (FileContent t) = do
    arrBuffer <- DOM.bsToArrayBuffer $ T.encodeUtf8 t
    f <- DOM.newFile [arrBuffer] ("mytext.txt" :: DOM.JSString) Nothing
    mkDataUrl (Just f)
mkFile' _ = pure (Nothing <$ never)

mkDataUrl :: (MonadWidget t m) => Maybe DOM.File -> m (Event t (Maybe Text))
mkDataUrl file = do 
    fileReader <- DOM.newFileReader
    DOM.readAsDataURL fileReader file 
    e <- wrapDomEvent fileReader (`DOM.on` DOM.loadEnd) . DOM.liftJSM $ do
            v <- fromMaybe (error "") <$> DOM.getResult fileReader
            s <- DOM.liftJSM $ (DOM.fromJSVal . DOM.unStringOrArrayBuffer) v
            pure $ (fmap T.pack s)
    pure $ e

dropWidget ::  MonadWidget t m => Event t Action -> m (El t, Dynamic t [DOM.File])
dropWidget dropEv = do
    uploaderText <- holdDyn initialUploadText $ ffor dropEv parseEventText 
    hasFile      <- holdDyn False (hasFileEventHanlder <$> dropEv)
    elAttr' "div" ("class" =: "js-upload uk-card-hover uk-placeholder uk-text-center uk-flex-center uk-card uk-card-default" <> "id" =: "uploader") $ do
        elAttr "span" ("uk-icon" =: "icon: cloud-upload") blank
        elAttr "span" ("class" =: "uk-text-middle") $ dynText uploaderText 
        fileSelectWidget hasFile
    where parseEventText Drop' = "Convert File..."
          parseEventText (FileContent filetxt) = filetxt
          parseEventText _     = initialUploadText
          hasFileEventHanlder (FileContent _) = True
          hasFileEventHanlder _           = False
          initialUploadText = " Attach binaries by dropping them here or "

fileSelectWidget :: MonadWidget t m => Dynamic t Bool -> m (Dynamic t [DOM.File])
fileSelectWidget hasFile = do
    elDynAttr "span" dynAttr $ do
        let fileDyn = value <$> fileInput def
        elAttr "span" ("class" =: "uk-link") $ text "select a file"
        fileDyn
    where dynAttr = ffor hasFile showSelector
          showSelector False = ("class" =: "uk-form-custom")
          showSelector True = ("hidden" =: mempty)

fileDownloaderWidget ::  MonadWidget t m => m ()
fileDownloaderWidget = do
    elAttr "p" ("class" =: "uk-card uk-card-default uk-width-1-1 uk-card-hover" <> "id" =: "file-download") $ text "No file uploaded."

dragOverHandler :: (MonadWidget t m) => (El t, a) -> m (Event t ())
dragOverHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    w <- wrapDomEvent rawEl (onEventName Dragover) $ do
            DOM.preventDefault
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
            DOM.setEffectAllowed dt ("all" :: DOM.JSString)
            DOM.setDropEffect dt ("copy" :: DOM.JSString)
    pure $ traceEvent "my e: " w
        
dropHandler :: (MonadWidget t m) => (El t, a) -> m (Event t (Maybe DOM.File))
dropHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    e <- wrapDomEvent rawEl (onEventName Drop) $ do 
            DOM.preventDefault
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
            DOM.setEffectAllowed dt ("all" :: DOM.JSString)
            flip DOM.item 0 =<< DOM.getFiles dt
    pure e
       

dragStartHandler :: (MonadWidget t m) => (El t, a) -> m (Event t ())
dragStartHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    wrapDomEvent rawEl (onEventName Dragstart) $ pure ()


readDropFile :: (MonadWidget t m) => Maybe DOM.File -> m (Event t Text)
readDropFile file = do 
    fileReader <- DOM.newFileReader
    DOM.readAsText fileReader file (Nothing :: Maybe DOM.JSString)
    e <- wrapDomEvent fileReader (`DOM.on` DOM.loadEnd) . DOM.liftJSM $ do
            v <- fromMaybe (error "") <$> DOM.getResult fileReader
            s <- DOM.liftJSM $ (DOM.fromJSVal . DOM.unStringOrArrayBuffer) v
            pure $ (fmap T.pack s)
    pure $ fmapMaybe id e


selectRawDiv :: (MonadWidget t m) => El t -> m DOM.HTMLDivElement
selectRawDiv div_ = DOM.unsafeCastTo DOM.HTMLDivElement (_element_raw div_)

holdDropEvent :: (MonadWidget t m) => Event t a -> (a -> m (Event t b)) -> m (Event t b)
holdDropEvent e f = do
    (_, e') <- runWithReplace (pure ()) (fmap f e)
    held <- hold never e'
    return (switch held)