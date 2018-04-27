{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (run, mainWidget)
import Data.Monoid ((<>))

main :: IO ()
main = run 3000 (mainWidget $ mainEntry)

mainEntry :: forall t m. MonadWidget t m => m ()
mainEntry = do
    dropEl <- elAttr' "div" ("style" =: "width: 250px; height: 250px; background-color: red; margin-bottom: 5px") $ blank
    dragEl <- elAttr' "div" ("style" =: "width: 250px; height: 250px; background-color: green; margin-bottom: 5px;" <> "draggable" =: "true") $ blank

    dragStartEv <- dragStartHandler dragEl
    dropOverEv  <- dragOverHandler dropEl 
    dropEv      <- dropHandler dropEl
    -- let rawdragDivEl = DOM.unsafeCastTo DOM.HTMLDivElement (_element_raw dragDiv)
    -- dragEv <- wrapDomEvent rawdragDivEl (`DOM.on` DOM.dragStart)
    dynText =<< (holdDyn "not dragged" $ leftmost [("dragged" <$ dragStartEv), ("dragged over" <$ dropOverEv), ("drop" <$ dropEv)])
    --display _<<
    pure ()


dragOverHandler :: forall t m a. MonadWidget t m => (El t, a) -> m (Event t ())
dragOverHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    w <- wrapDomEvent rawEl (onEventName Dragover) $ do
            DOM.preventDefault
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
            DOM.setEffectAllowed dt ("all" :: JSString)
            DOM.setDropEffect dt ("copy" :: JSString)
    pure $ traceEvent "my e: " w
        
dropHandler :: forall t m a. MonadWidget t m => (El t, a) -> m (Event t Text)
dropHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    e <- wrapDomEvent rawEl (onEventName Drop) $ do 
            DOM.preventDefault
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
            DOM.setEffectAllowed dt ("all" :: JSString)
            file <- (flip DOM.item 0) =<< DOM.getFiles dt
            readDropFile file
    pure $ coincidence e
    
       

dragStartHandler :: forall t m a. MonadWidget t m => (El t, a) -> m (Event t ())
dragStartHandler rEl = do
    rawEl <- selectRawDiv . fst $ rEl 
    wrapDomEvent rawEl (onEventName Dragstart) $ pure ()


readDropFile :: forall t m. MonadWidget t m => Maybe DOM.File -> m (Event t Text)
readDropFile file = pure never
    -- do 
    -- fileReader <- DOM.newFileReader
    -- DOM.readAsText fileReader file (Nothing :: Maybe JSString)
    -- e <- wrapDomEvent fileReader (`DOM.on` DOM.loadEnd) . DOM.liftJSM $ do
    --         v <- fromMaybe (error "") <$> DOM.getResult fileReader
    --         s <- DOM.liftJSM $ (DOM.fromJSVal . DOM.unStringOrArrayBuffer) v
    --         pure $ (fmap T.pack s)
    -- pure $ fmapMaybe id e


selectRawDiv :: forall t m. MonadWidget t m => El t -> m DOM.HTMLDivElement
selectRawDiv div_ = DOM.unsafeCastTo DOM.HTMLDivElement (_element_raw div_)








--- Other way of getting File, However output same error as above  ---
s 
-- main :: IO ()
-- main = run 3000 (mainWidget $ mainEntry)

-- mainEntry :: MonadWidget t m => m ()
-- mainEntry = do
--     dropEl <- elAttr' "div" ("style" =: "width: 250px; height: 250px; background-color: red; margin-bottom: 5px") $ blank
--     dragEl <- elAttr' "div" ("style" =: "width: 250px; height: 250px; background-color: green; margin-bottom: 5px;" <> "draggable" =: "true") $ blank

--     dragStartEv <- dragStartHandler dragEl
--     dropOverEv  <- dragOverHandler dropEl 
--     dropEv      <- dropHandler dropEl
--     let m = coincidence $ dropEv >>= readDropFile
--     -- let rawdragDivEl = DOM.unsafeCastTo DOM.HTMLDivElement (_element_raw dragDiv)
--     -- dragEv <- wrapDomEvent rawdragDivEl (`DOM.on` DOM.dragStart)
--     dynText =<< (holdDyn "not dragged" $ leftmost [("dragged" <$ dragStartEv), ("dragged over" <$ dropOverEv)])
--     --display _<<
--     pure ()


-- dragOverHandler :: (MonadWidget t m) => (El t, a) -> m (Event t ())
-- dragOverHandler rEl = do
--     rawEl <- selectRawDiv . fst $ rEl 
--     w <- wrapDomEvent rawEl (onEventName Dragover) $ do
--             DOM.preventDefault
--             dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
--             DOM.setEffectAllowed dt ("all" :: JSString)
--             DOM.setDropEffect dt ("copy" :: JSString)
--     pure $ traceEvent "my e: " w
        
-- dropHandler :: (MonadWidget t m) => (El t, a) -> m (Event t DOM.FileReader)
-- dropHandler rEl = do
--     rawEl <- selectRawDiv . fst $ rEl 
--     e <- wrapDomEvent rawEl (onEventName Drop) $ do 
--             DOM.preventDefault
--             dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event) 
--             DOM.setEffectAllowed dt ("all" :: JSString)
--             file <- (flip DOM.item 0) =<< DOM.getFiles dt
--             fileReader <- DOM.newFileReader
--             DOM.readAsText fileReader file (Nothing :: Maybe JSString)
--             pure fileReader
--             -- e <- wrapDomEvent fileReader (`DOM.on` DOM.loadEnd) . DOM.liftJSM $ do
--             --         v <- fromMaybe (error "") <$> DOM.getResult fileReader
--             --         s <- DOM.liftJSM $ (DOM.fromJSVal . DOM.unStringOrArrayBuffer) v
--             --         pure $ (fmap T.pack s)
--             -- pure $ fmapMaybe id e
--     pure e
       

-- dragStartHandler :: (MonadWidget t m) => (El t, a) -> m (Event t ())
-- dragStartHandler rEl = do
--     rawEl <- selectRawDiv . fst $ rEl 
--     wrapDomEvent rawEl (onEventName Dragstart) $ pure ()


-- readDropFile :: (MonadWidget t m) => DOM.FileReader -> m (Event t Text)
-- readDropFile fileReader = do 
--     DOM.readAsText fileReader file (Nothing :: Maybe JSString)
--     e <- wrapDomEvent fileReader (`DOM.on` DOM.loadEnd) . DOM.liftJSM $ do
--             v <- fromMaybe (error "") <$> DOM.getResult fileReader
--             s <- DOM.liftJSM $ (DOM.fromJSVal . DOM.unStringOrArrayBuffer) v
--             pure $ (fmap T.pack s)
--     pure $ fmapMaybe id e


-- selectRawDiv :: (MonadWidget t m) => El t -> m DOM.HTMLDivElement
-- selectRawDiv div_ = DOM.unsafeCastTo DOM.HTMLDivElement (_element_raw div_)