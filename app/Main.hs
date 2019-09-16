{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async       (async)
import           Data.Text                      ( Text )
import           Control.Monad                  ( void )
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified GI.Gdk                         as Gdk
import           GI.Gtk                         ( Window (..), TextView (..))
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Paths_bene

data State = Initial

data Event = Closed

view' :: State -> AppView Window Event
view' s =
  bin
      Window
      [ #title := "Bene"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ case s of
        Initial -> widget TextView []

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

loadStylesheet :: String -> IO ByteString
loadStylesheet = B.readFile


main :: IO ()
main = do
  void $ Gtk.init Nothing

  f <- getDataFileName "themes/test.css"

  print f

  css <- loadStylesheet f

  screen <- maybe (fail "No screen?") return =<< Gdk.screenGetDefault
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData provider css
  Gtk.styleContextAddProviderForScreen
    screen
    provider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)


  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
  where
    app = App { view         = view'
              , update       = update'
              , inputs       = []
              , initialState = Initial
              }
