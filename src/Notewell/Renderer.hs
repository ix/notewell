{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
 Module : Notewell.Renderer
 Description : Renders CMark node trees as GTK widgets.
 Copyright : Rose <rose@empty.town>
 License : BSD3
 Maintainer : rose@empty.town
-}

module Notewell.Renderer where

import CMarkGFM
import Control.Monad
import Data.Maybe       (catMaybes, fromMaybe)
import Data.Text        (Text)
import Notewell.Helpers
import Notewell.Theming

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T

import GI.Gtk hiding (Justification, Scale, Style, Weight)

-- | Clear a TextBuffer of all styling tags.
removeAllTags :: TextBuffer -> IO ()
removeAllTags textBuffer = do
  startIter <- #getStartIter textBuffer
  endIter   <- #getEndIter textBuffer
  #removeAllTags textBuffer startIter endIter

-- | Apply Markdown styling on the full content of a TextBuffer.
applyAllTags :: TextBuffer -> IO ()
applyAllTags textBuffer = do
  maybeContent <- get textBuffer #text
  whenM maybeContent $ \content ->
    applyNode textBuffer $ commonmarkToNode [] [extStrikethrough] content

-- | Apply a TextTag to a TextBuffer at the location from a PosInfo.
-- TextIter begins at 0 whereas PosInfo begins at 1, so we decrement.
applyTag :: TextBuffer -> PosInfo -> Text -> IO ()
applyTag buffer (PosInfo sl' sc' el' ec') tag = do
  s <- #getIterAtLineOffset buffer sl sc
  e <- #getIterAtLineOffset buffer el ec
  #applyTagByName buffer tag s e
 where
  sl = fromIntegral $ pred sl'
  sc = fromIntegral $ pred sc'
  el = fromIntegral $ pred el'
  ec = fromIntegral ec'

-- | Adjust the PosInfo of the text & url components of a LINK node.
-- This is used for providing nicer rendering.
adjustLinkPosInfo :: (PosInfo, PosInfo) -> (PosInfo, PosInfo)
adjustLinkPosInfo (textPos, urlPos) = (textPos, urlPos')
  where urlPos' = urlPos { startColumn = endColumn textPos + 3
                         , endColumn   = endColumn urlPos - 1 }

-- | Traverses a Node and applies formatting tags to a buffer accordingly.
applyNode :: TextBuffer -> Node -> IO ()
applyNode buffer (Node (Just pos) EMPH children) = do
  applyTag buffer pos "emph"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) STRONG children) = do
  applyTag buffer pos "strong"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) (CODE _        ) _       ) = applyTag buffer pos "code"
applyNode buffer (Node (Just pos) (CODE_BLOCK _ _) _       ) = applyTag buffer pos "codeBlock"
applyNode buffer (Node (Just pos) (HEADING level ) children) = do
  applyTag buffer pos $ T.concat ["heading", T.pack $ show level]
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just urlPos) (LINK url _) [Node (Just textPos) (TEXT content) _]) = do
  applyTag buffer textPos "linkText"
  applyTag buffer (snd $ adjustLinkPosInfo (textPos, urlPos)) "linkUrl"
applyNode buffer (Node (Just pos) STRIKETHROUGH children) = do
  applyTag buffer pos "strikethrough"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) THEMATIC_BREAK children) = do
  applyTag buffer pos "thematicBreak"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) (LIST _) children) = do
  applyTag buffer pos "list"
  mapM_ (applyNode buffer) children
applyNode buffer (Node (Just pos) BLOCK_QUOTE children) = do
  applyTag buffer pos "blockquote"
  mapM_ (applyNode buffer) children
applyNode _      (Node _ (TEXT _) _       ) = pure ()
applyNode buffer (Node _ _        children) = mapM_ (applyNode buffer) children

-- | Build a TextTagTable from a Theme.
markdownTextTagTable :: Theme -> IO TextTagTable
markdownTextTagTable theme = do
  table <- textTagTableNew
  populateTable table theme
  pure table

-- | Add tags for a Theme to a TextTagTable.
populateTable :: TextTagTable -> Theme -> IO ()
populateTable table theme = do
  mapM_ (textTagTableAdd table <=< uncurry mkTag) $ HM.toList $ elements theme
  mapM_ ((textTagTableAdd table =<<) . mkHeadingTag heading) [1..6]
 where heading = fromMaybe mempty $ HM.lookup "heading" $ elements theme

-- | Remove all tags for a theme from a TextTagTable.
clearTable :: TextTagTable -> Theme -> IO ()
clearTable table theme =
  forM_ tagNames $ \name -> do
  maybeTag <- textTagTableLookup table name
  whenM maybeTag $ textTagTableRemove table
  where tagNames = HM.keys $ elements theme

-- | Given a name and some TagProperties, produce a TextTag.
mkTag :: Text -> TagProperties -> IO TextTag
mkTag name properties = do
  tag <- textTagNew $ Just name
  mapM_ (`apply` tag) properties
  pure tag
  where
    apply :: TagProperty -> TextTag -> IO ()
    apply (Color color)        = flip setTextTagForeground color
    apply (Font font)          = flip setTextTagFamily font
    apply (Scale scale)        = flip setTextTagScale scale
    apply (Style style)        = flip setTextTagStyle style
    apply (Weight weight)      = flip setTextTagWeight (fromIntegral $ fromEnum weight)
    apply (Indent indent)      = flip setTextTagIndent indent
    apply (Strike strike)      = flip setTextTagStrikethrough strike
    apply (Justification j11n) = flip setTextTagJustification j11n
    apply (Underline ul)       = flip setTextTagUnderline ul

-- | Create a heading tag of a given level.
mkHeadingTag :: TagProperties -> Level -> IO TextTag
mkHeadingTag properties level = do
  tag <- mkTag (T.pack $ "heading" ++ show level) properties
  setTextTagScale tag $ scaling level
  pure tag
 where
  scaling n | n <= 1    = 3
            | n == 2    = 2.5
            | n == 3    = 2
            | n == 4    = 1.5
            | n == 5    = 1.25
            | otherwise = 1

-- | /O(n)/ Return the length (in bytes) of a Text string.
bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
