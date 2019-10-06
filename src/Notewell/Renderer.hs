{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{- |
 Module : Notewell.Renderer
 Description : Renders CMark node trees as GTK widgets.
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

module Notewell.Renderer where

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
 where
  sl = fromIntegral $ pred sl'
  sc = fromIntegral $ pred sc'
  el = fromIntegral $ pred el'
  ec = fromIntegral $ ec'

-- | Traverses a Node and applies formatting tags to a buffer accordingly.
applyNode :: Gtk.TextBuffer -> Node -> IO ()
applyNode buffer (Node (Just pos) EMPH children) = do
  applyTag buffer pos "emph"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) STRONG children) = do
  applyTag buffer pos "strong"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) (CODE t) _) = applyTag buffer pos "code"
applyNode buffer (Node (Just pos) (CODE_BLOCK _ t) _) =
  applyTag buffer pos "codeblock"
applyNode buffer (Node (Just pos) (HEADING level) children) = do
  applyTag buffer pos $ T.concat ["heading", T.pack $ show level]
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) STRIKETHROUGH children) = do
  applyTag buffer pos "strikethrough"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) THEMATIC_BREAK children) = do
  applyTag buffer pos "thematicbreak"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) (LIST _) children) = do
  applyTag buffer pos "list"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) BLOCK_QUOTE children) = do
  applyTag buffer pos "blockquote"
  mapM_ (applyNode buffer) children
applyNode _      (Node _ (TEXT t) _       ) = return ()
applyNode buffer (Node _ _        children) = mapM_ (applyNode buffer) children

-- | Parse the Markdown contained in a TextBuffer to a Node tree
-- and proceed to render it using TextTags.
formatBuffer :: Gtk.TextBuffer -> IO ()
formatBuffer buffer = do
  s       <- Gtk.textBufferGetStartIter buffer
  e       <- Gtk.textBufferGetEndIter buffer
  content <- Gtk.textBufferGetText buffer s e True
  applyNode buffer (commonmarkToNode [] [extStrikethrough] content)

-- | Build a TextTagTable from our TextTags.
-- Note that Markdown uses HTML headings, and so exactly six levels h1-h6.
markdownTextTagTable :: IO Gtk.TextTagTable
markdownTextTagTable = do
  table <- Gtk.textTagTableNew
  mapM_ (Gtk.textTagTableAdd table =<<) tags
  return table
 where
  tags =
    [ emph
      , strong
      , code
      , codeBlock
      , strikethrough
      , thematicBreak
      , list
      , blockquote
      ]
      ++ map heading [1 .. 6]

heading :: Int -> IO Gtk.TextTag
heading level = do
  tag <- Gtk.textTagNew $ Just $ T.concat ["heading", T.pack $ show level]
  Gtk.setTextTagWeight tag $ fromIntegral $ fromEnum Pango.WeightHeavy
  Gtk.setTextTagScale tag $ if
    | level <= 1 -> 2
    | level == 2 -> 1.75
    | level == 3 -> 1.5
    | level == 4 -> 1.25
    | level >= 5 -> 1
  return tag

emph :: IO Gtk.TextTag
emph = do
  tag <- Gtk.textTagNew $ Just "emph"
  Gtk.setTextTagStyle tag Pango.StyleItalic
  return tag

strong :: IO Gtk.TextTag
strong = do
  tag <- Gtk.textTagNew $ Just "strong"
  Gtk.setTextTagWeight tag $ fromIntegral $ fromEnum Pango.WeightHeavy
  return tag

code :: IO Gtk.TextTag
code = do
  tag <- Gtk.textTagNew $ Just "code"
  Gtk.setTextTagFamily tag "monospace"
  Gtk.setTextTagScale tag 0.75
  return tag

codeBlock :: IO Gtk.TextTag
codeBlock = do
  tag <- Gtk.textTagNew $ Just "codeblock"
  Gtk.setTextTagFamily tag "monospace"
  Gtk.setTextTagScale tag 0.75
  return tag

strikethrough :: IO Gtk.TextTag
strikethrough = do
  tag <- Gtk.textTagNew $ Just "strikethrough"
  Gtk.setTextTagStrikethrough tag True
  return tag

thematicBreak :: IO Gtk.TextTag
thematicBreak = do
  tag <- Gtk.textTagNew $ Just "thematicbreak"
  Gtk.setTextTagJustification tag Gtk.JustificationCenter
  return tag

list :: IO Gtk.TextTag
list = do
  tag <- Gtk.textTagNew $ Just "list"
  Gtk.setTextTagIndent tag 30
  return tag

blockquote :: IO Gtk.TextTag
blockquote = do
  tag <- Gtk.textTagNew $ Just "blockquote"
  Gtk.setTextTagVariant tag Pango.VariantSmallCaps
  Gtk.setTextTagStyle tag Pango.StyleItalic
  Gtk.setTextTagIndent tag 30
  return tag

-- | /O(n)/ Return the length (in bytes) of a Text string.
bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
