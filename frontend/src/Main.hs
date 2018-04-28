{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.JSString (JSString)
import qualified Data.Aeson as AE
import qualified GHCJS.DOM.DataTransfer         as DOM
import qualified GHCJS.DOM.FileReader           as DOM
import qualified GHCJS.DOM.FileList             as DOM
import qualified GHCJS.DOM.File                 as DOM
import qualified GHCJS.DOM.Element              as DOM
import qualified GHCJS.DOM.EventM               as DOM
import qualified GHCJS.DOM.MouseEvent           as DOM
import qualified GHCJS.DOM.Types                as DOM 
-- import Language.Javascript.JSaddle.Warp (run)
import Dev
import Reflex.Dom.Core (mainWidget, mainWidgetWithHead)
import Reflex.Dom hiding (run, mainWidget, mainWidgetWithHead)
import Data.Monoid ((<>))


--- Other way of getting File, However output same error as above  ---

main :: IO ()
main = run mainEntry

headWidget :: MonadWidget t m => m ()
headWidget = do
    elAttr "meta"   ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
    elAttr "link"   ("href" =: "../css/uikit.min.css")    blank
    elAttr "script" ("src" =: "../js/uikit.min.js")       blank
    elAttr "script" ("src" =: "../js/uikit-icons.min.js") blank


mainEntry :: MonadWidget t m => m ()
mainEntry = 
    elAttr "div" ("class" =: "widget-wrapper uk-height-1-1 ui-flex uk-flex-middle uk-flex-center uk-text-center") mainEntry'

dropWidget ::  MonadWidget t m => Behavior t (Event t Text) -> m (El t, ())
dropWidget bEv = do
    let uploaderText = dynText =<< (holdDyn " Attach binaries by dropping them here or" $ switch bEv)
    elAttr' "div" ("class" =: "uk-card uk-card-default uk-card-hover uk-placeholder" <> "id" =: "uploader") $ do
        elAttr "span" ("uk-icon" =: "icon: cloud-upload") blank
        elAttr "span" ("class" =: "uk-text-middle") $ uploaderText 

mainEntry' :: MonadWidget t m => m ()
mainEntry' = mdo
    dropEl <- dropWidget (constant $ leftmost ["Converting File..." <$ dropEv, fileText]) <-- and this
    -- dragEl <- elAttr' "div" ("style" =: "width: 250px; height: 250px; background-color: green; margin-bottom: 5px;" <> "draggable" =: "true") $ blank
    -- dragStartEv <- dragStartHandler dragEl

    dropOverEv  <- dragOverHandler dropEl 
    dropEv      <- dropHandler dropEl
    fileText    <- holdDropEvent dropEv readDropFile
    dynText =<< (holdDyn "not dragged" $ leftmost [("dragged over" <$ dropOverEv), fileText]) <-- need to fix this
    pure ()


dragOverHandler :: (MonadWidget t m) => (El t, a) -> m (Event t ())
dragOverHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    w <- wrapDomEvent rawEl (onEventName Dragover) $ do
            DOM.preventDefault
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
            DOM.setEffectAllowed dt ("all" :: JSString)
            DOM.setDropEffect dt ("copy" :: JSString)
    pure $ traceEvent "my e: " w
        
dropHandler :: (MonadWidget t m) => (El t, a) -> m (Event t (Maybe DOM.File))
dropHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    e <- wrapDomEvent rawEl (onEventName Drop) $ do 
            DOM.preventDefault
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
            DOM.setEffectAllowed dt ("all" :: JSString)
            flip DOM.item 0 =<< DOM.getFiles dt
    pure e
       

dragStartHandler :: (MonadWidget t m) => (El t, a) -> m (Event t ())
dragStartHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    wrapDomEvent rawEl (onEventName Dragstart) $ pure ()


readDropFile :: (MonadWidget t m) => Maybe DOM.File -> m (Event t Text)
readDropFile file = do 
    fileReader <- DOM.newFileReader
    DOM.readAsText fileReader file (Nothing :: Maybe JSString)
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