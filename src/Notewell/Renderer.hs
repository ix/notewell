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

import           Notewell.Theming

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

-- | Build a TextTagTable from a Theme.
markdownTextTagTable :: Theme -> IO Gtk.TextTagTable
markdownTextTagTable theme = do
  table <- Gtk.textTagTableNew
  mapM_ (Gtk.textTagTableAdd table <=< uncurry mkTag) zipped
  mapM_ (Gtk.textTagTableAdd table =<<) $ map (mkHeadingTag $ heading theme) [1..6]
  return table
 where
  zipped = zip (map Just names) (map ($ theme) constructors)
  names =
    [ "emph"
    , "strong"
    , "code"
    , "codeBlock"
    , "strikethrough"
    , "thematicBreak"
    , "list"
    , "blockquote"
    ]
  constructors =
    [ emph
    , strong
    , code
    , codeBlock
    , strikethrough
    , thematicBreak
    , list
    , blockquote
    ]

-- | Given an optional name and some TagProperties, produce a TextTag.
mkTag :: Maybe Text -> TagProperties -> IO Gtk.TextTag
mkTag name properties = do
  tag <- Gtk.textTagNew name
  whenJust (font properties) $ Gtk.setTextTagFamily tag
  whenJust (color properties) $ Gtk.setTextTagForeground tag
  whenJust (scale properties) $ Gtk.setTextTagScale tag
  whenJust (style properties) $ Gtk.setTextTagStyle tag
  whenJust (weight properties)
    $ Gtk.setTextTagWeight tag
    . fromIntegral
    . fromEnum
  whenJust (indent properties) $ Gtk.setTextTagIndent tag
  return tag
  where whenJust m f = maybe (return ()) f m

-- | Create a heading tag of a given level.
mkHeadingTag :: TagProperties -> Level -> IO Gtk.TextTag
mkHeadingTag properties level = do
  tag <- mkTag (Just $ T.pack $ "heading" ++ show level) properties
  Gtk.setTextTagScale tag $ scaling $ fromIntegral level 
  return tag
  where scaling n
          | n <= 1    = 3
          | n == 2    = 2.5
          | n == 3    = 2
          | n == 4    = 1.5
          | n == 5    = 1.25
          | otherwise = 1

-- | /O(n)/ Return the length (in bytes) of a Text string.
bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
