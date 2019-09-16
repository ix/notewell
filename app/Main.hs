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
                                                , FileChooserButton(..)
                                                , Button(..)
                                                , Label(..)
                                                , fileChooserGetFilename
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Fileless | FileOpened FilePath

data Event = Closed | FileSelected (Maybe FilePath)

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
        Fileless -> widget
          FileChooserButton
          [onM #selectionChanged (fmap FileSelected . fileChooserGetFilename)]
        FileOpened file -> widget Label [#label := pack file]

update' :: State -> Event -> Transition State Event
update' _ (FileSelected (Just file)) =
  Transition (FileOpened file) (return Nothing)
update' s (FileSelected Nothing) = Transition s (return Nothing)
update' _ Closed                 = Exit

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = Fileless
                      }
