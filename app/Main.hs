{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Control.Monad                 (void)

import           GI.Gtk                        (Label (..), Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial | Greeting Text

data Event = Greet Text | Closed

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Hello", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ case s of
        Initial      -> widget Label [#label := "Nothing here yet."]
        Greeting who -> widget Label [#label := who]

update' :: State -> Event -> Transition State Event
update' _ (Greet who) = Transition (Greeting "hi") (return Nothing)
update' _ Closed      = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = Initial
  }
