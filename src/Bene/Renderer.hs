{-# LANGUAGE OverloadedStrings #-}
{- |
 Module : Bene.Renderer
 Description : Renders CMark node trees as GTK widgets.
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

module Bene.Renderer where

import qualified Data.ByteString               as BS

import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as T

import           CMarkGFM
import qualified Data.Text                     as T

import qualified GI.Gtk                        as Gtk
import qualified GI.Pango.Enums                as Pango

import           Control.Monad

-- | Apply a TextTag to a TextBuffer at the location from a PosInfo.
-- TextIter begins at 0 whereas PosInfo begins at 1, so we decrement.
applyTag :: Gtk.TextBuffer -> PosInfo -> Text -> IO ()
applyTag buffer (PosInfo sl' sc' el' ec') tag = do
  s <- Gtk.textBufferGetIterAtLineOffset buffer sl sc
  e <- Gtk.textBufferGetIterAtLineOffset buffer el ec
  Gtk.textBufferApplyTagByName buffer tag s e
  return ()
  where sl = fromIntegral $ pred sl'
        sc = fromIntegral $ pred sc'
        el = fromIntegral $ pred el'
        ec = fromIntegral $ pred ec'

-- | Traverses a Node and applies formatting tags to a buffer accordingly.
applyNode :: Gtk.TextBuffer -> Node -> IO ()
applyNode buffer (Node (Just pos) EMPH children) = do
  applyTag buffer pos "emph"
  mapM_ (applyNode buffer) children
applyNode _ (Node _ (TEXT t) _) = return ()
applyNode buffer (Node _ _ children) = mapM_ (applyNode buffer) children

formatBuffer :: Gtk.TextBuffer -> IO ()
formatBuffer buffer = do
  s <- Gtk.textBufferGetStartIter buffer 
  e <- Gtk.textBufferGetEndIter buffer
  content <- Gtk.textBufferGetText buffer s e True
  applyNode buffer (commonmarkToNode [] [] content)

markdownTextTagTable :: IO Gtk.TextTagTable
markdownTextTagTable = do
  table <- Gtk.textTagTableNew
  mapM_ (Gtk.textTagTableAdd table =<<) [emph, strong]
  return table

emph :: IO Gtk.TextTag
emph = do
  tag <- Gtk.textTagNew $ Just "emph"
  Gtk.setTextTagStyle tag Pango.StyleItalic
  return tag

strong :: IO Gtk.TextTag
strong = do
  tag <- Gtk.textTagNew $ Just "strong"
  Gtk.setTextTagStyle tag Pango.StyleOblique
  return tag

bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
