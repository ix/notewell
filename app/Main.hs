{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Control.Monad                  ( void )
import           GI.Gtk                         ( Window(..)
                                                , TextView(..)
                                                , FileChooserDialog(..)
                                                , ToolButton(..)
                                                , Box(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , Align(..)
                                                , ScrolledWindow(..)
                                                , fileChooserGetFilename
                                                )
import           Control.Concurrent.Async       ( async )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Paths_bene

data State = Welcome | FileOpened FilePath | Editing (Maybe Text)

data Event = Closed | FileSelected (Maybe FilePath) | NewDocument

view' :: State -> AppView Window Event
view' s =
  bin
      Window
      [ #title := "Bene"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 480
      , #heightRequest := 300
      ]
    $ case s of
        Welcome ->
          container Box [#orientation := OrientationHorizontal]
            $ [ expandableChild $ widget
                ToolButton
                [ #iconName := "document-new"
                , classes ["intro"]
                , on #clicked NewDocument
                ]
              , expandableChild $ widget
                ToolButton
                [#iconName := "document-open", classes ["intro"]]
              ]
        FileOpened file -> widget Label [#label := pack file]
        Editing    _    -> bin ScrolledWindow [] $ widget TextView [#wrapMode := Gtk.WrapModeWord, classes ["editor"]]

expandableChild :: Widget a -> BoxChild a
expandableChild =
  BoxChild defaultBoxChildProperties { expand = True, fill = True }

update' :: State -> Event -> Transition State Event
update' _ (FileSelected (Just file)) =
  Transition (FileOpened file) (return Nothing)
update' s (FileSelected Nothing) = Transition s (return Nothing)
update' _ NewDocument            = Transition (Editing Nothing) (return Nothing)
update' _ Closed                 = Exit

main :: IO ()
main = do
  void $ Gtk.init Nothing

  path     <- pack <$> getDataFileName "themes/giorno/giorno.css"

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
