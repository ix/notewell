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
import Bene.Renderer (bytes)
import           Paths_bene

data State = Welcome | FileSelection | Blank | Editing (IO Gtk.TextBuffer)

data Event = Closed | FileSelected (Maybe FilePath) | NewDocument | OpenDocument

bufferFromFile :: FilePath -> IO Gtk.TextBuffer
bufferFromFile filename = do
  buffer   <- Gtk.textBufferNew Gtk.noTextTagTable
  contents <- T.readFile filename
  Gtk.textBufferSetText buffer contents $ fromIntegral $ bytes contents
  return buffer

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
          [onM #fileActivated (fmap FileSelected . Gtk.fileChooserGetFilename)]
        Blank -> bin Gtk.ScrolledWindow []
          $ widget
              Gtk.TextView
              [#wrapMode := Gtk.WrapModeWord, classes ["editor"]]
        Editing buffer -> bin Gtk.ScrolledWindow [] $ widget
          Gtk.TextView
          [ afterCreated $ \tv -> Gtk.textViewSetBuffer tv . Just =<< buffer
          , #wrapMode := Gtk.WrapModeWord
          , classes ["editor"]
          ]

expandableChild :: Widget a -> BoxChild a
expandableChild =
  BoxChild defaultBoxChildProperties { expand = True, fill = True }

update' :: State -> Event -> Transition State Event
update' _ (FileSelected (Just file)) =
  Transition (Editing $ bufferFromFile file) (return Nothing)
update' s (FileSelected Nothing) = Transition s (return Nothing)
update' _ NewDocument            = Transition Blank (return Nothing)
update' _ OpenDocument           = Transition FileSelection (return Nothing)
update' _ Closed                 = Exit

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
