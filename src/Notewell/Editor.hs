{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
  Module : Notewell.Editor
  Description : A declarative Markdown editor widget.
  Copyright : rose@empty.town
  License : BSD3
  Maintainer : rose@empty.town
-}

module Notewell.Editor where


import CMarkGFM                       (commonmarkToNode, extStrikethrough)
import Control.Monad                  ((<=<))
import Data.Text                      (Text)
import Data.Vector                    (Vector)
import GI.GObject                     (signalHandlerDisconnect)
import GI.Gtk
import GI.Gtk.Declarative.EventSource (fromCancellation)

import qualified GI.Gtk.Declarative        as D
import qualified GI.Gtk.Declarative.State  as D
import qualified GI.Gtk.Declarative.Widget as D

import Notewell.Events
import Notewell.Helpers
import Notewell.Metrics
import Notewell.Renderer
import Notewell.Theming

data EditorParams = EditorParams { editorTheme  :: Theme        -- ^ The editor theme to use (used when creating the tag table).
                                 , editorBuffer :: TextBuffer } -- ^ The application-global buffer.

-- | Attempt to convert a declarative Widget to its "raw" GTK representation.
transmute :: (GObject o, D.Patchable widget) => (ManagedPtr o -> o) -> widget e -> IO (Maybe o)
transmute constructor = castTo constructor <=< D.someStateWidget <=< D.create

-- | Constructs a custom declarative Markdown Editor widget.
markdownEditor :: Vector (D.Attribute TextView Event) -> EditorParams -> D.Widget Event
markdownEditor attrs params = D.Widget $ D.CustomWidget { customAttributes = attrs, customParams = params, .. }
 where
  customWidget = TextView
  customCreate EditorParams {..} = do
    textView <- new TextView []
    #setBuffer textView $ Just editorBuffer
    return (textView, editorBuffer)
  customPatch _ _ _ = D.CustomKeep
  customSubscribe _ (buffer :: TextBuffer) (textView :: TextView) f = do
    counts  <- count buffer
    editShId <- on buffer #changed $ f $ UpdateMetrics $ Metrics { counts = counts }
    return $ fromCancellation $ signalHandlerDisconnect buffer editShId
