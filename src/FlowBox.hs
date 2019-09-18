{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GI.Gtk.Declarative.Container.ListBox where

import           Data.Vector                        (Vector)
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container.Class

instance IsContainer Gtk.FlowBox (Bin Gtk.FlowBoxChild) where
  appendChild box _ widget' =
    Gtk.flowBoxInsert box widget' (-1)
  replaceChild box _ i old new = do
    Gtk.widgetDestroy old
    Gtk.flowBoxInsert box new i

instance ToChildren Gtk.FlowBox Vector (Bin Gtk.FlowBoxChild)
