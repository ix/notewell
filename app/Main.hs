{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import           Control.Monad
import           Control.Concurrent.Async       ( async )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import qualified Data.ByteString.Char8         as BS
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Bene.Renderer
import           Paths_bene

data State = Welcome | FileSelection | Editing (IO Gtk.TextBuffer)

data Event = Closed | FileSelected (Maybe FilePath) | NewDocument | OpenDocument | Typed

-- | Create a TextBuffer from an optional filepath.
-- If one is provided, the buffer is populated
-- with the file's contents.
createBuffer :: Maybe FilePath -> IO Gtk.TextBuffer
createBuffer maybeFile = do
  buffer <- Gtk.textBufferNew . Just =<< markdownTextTagTable
  Gtk.on buffer #endUserAction (renderMarkdown buffer)
  case maybeFile of
    Nothing       -> return buffer
    Just filepath -> do
      contents <- T.readFile filepath
      Gtk.textBufferSetText buffer contents $ fromIntegral $ bytes contents
      return buffer

-- | Constructs a FileFilter which pertains to Markdown documents.
markdownFileFilter :: IO Gtk.FileFilter
markdownFileFilter = do
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterSetName filt $ Just "Markdown documents"
  Gtk.fileFilterAddMimeType filt "text/markdown"
  Gtk.fileFilterAddMimeType filt "text/x-markdown"
  Gtk.fileFilterAddPattern filt "**/*.{markdown,md}"
  return filt

-- | Create a TextView widget from a given TextBuffer.
editor :: IO Gtk.TextBuffer -> Widget Event
editor buffer = widget
  Gtk.TextView
  [ afterCreated setBuffer
  , #wrapMode := Gtk.WrapModeWord
  , #margin := 10
  , classes ["editor"]
  ]
  where setBuffer tv = Gtk.textViewSetBuffer tv . Just =<< buffer

-- | Render Markdown based on the content of a TextBuffer.
renderMarkdown :: Gtk.TextBuffer -> IO ()
renderMarkdown buffer = clearTags buffer >> formatBuffer buffer

-- | Remove all TextTags from a buffer.
clearTags :: Gtk.TextBuffer -> IO ()
clearTags buffer = do
  s <- Gtk.textBufferGetStartIter buffer
  e <- Gtk.textBufferGetEndIter buffer
  Gtk.textBufferRemoveAllTags buffer s e

-- | Place a widget inside a BoxChild and allow it to expand.
expandableChild :: Widget a -> BoxChild a
expandableChild =
  BoxChild defaultBoxChildProperties { expand = True, fill = True }

view' :: State -> AppView Gtk.Window Event
view' s =
  bin
      Gtk.Window
      [ #title := "Bene"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 480
      , #heightRequest := 300
      ]
    $ case s of
        Welcome ->
          container Gtk.Box [#orientation := Gtk.OrientationHorizontal]
            $ [ expandableChild $ widget
                Gtk.ToolButton
                [ #iconName := "document-new"
                , on #clicked NewDocument
                , classes ["intro"]
                ]
              , expandableChild $ widget
                Gtk.ToolButton
                [ #iconName := "document-open"
                , on #clicked OpenDocument
                , classes ["intro"]
                ]
              ]
        FileSelection -> widget
          Gtk.FileChooserWidget
          [ afterCreated
            $ \fc -> Gtk.fileChooserAddFilter fc =<< markdownFileFilter
          , onM #fileActivated (fmap FileSelected . Gtk.fileChooserGetFilename)
          ]
        Editing buffer -> bin Gtk.ScrolledWindow [] $ editor buffer

update' :: State -> Event -> Transition State Event
update' _ (FileSelected (Just file)) =
  Transition (Editing $ createBuffer $ Just file) (return Nothing)
update' s (FileSelected Nothing) = Transition s (return Nothing)
update' _ NewDocument =
  Transition (Editing $ createBuffer Nothing) (return Nothing)
update' _ OpenDocument = Transition FileSelection (return Nothing)
update' s Typed        = Transition s (return Nothing)
update' _ Closed       = Exit

main :: IO ()
main = do
  void $ Gtk.init Nothing

  path     <- T.pack <$> getDataFileName "themes/giorno/giorno.css"

  screen   <- maybe (fail "No screen?") return =<< Gdk.screenGetDefault
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider path
  Gtk.styleContextAddProviderForScreen
    screen
    provider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
 where
  app =
    App { view = view', update = update', inputs = [], initialState = Welcome }
