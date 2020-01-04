{-# LANGUAGE OverloadedStrings #-}

{- |
 Module : Notewell.Metrics
 Description : Analysis functions for GTK TextBuffer.
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

module Notewell.Metrics where

import           GI.Gtk

import qualified Data.Text as T

-- | Data gathered from analyzing a TextBuffer.
newtype Metrics = Metrics { counts :: (Int, Int, Int) }

-- | Default buffer metrics.
empty :: Metrics
empty = Metrics (0, 1, 0)

-- | Format word, line and column counts from Metrics as human-readable.
formatCounts :: Metrics -> T.Text
formatCounts (Metrics (ws', ls', cs')) = mconcat [ws, " words, ", ls, " lines, ", cs, " chars"]
  where (ws, ls, cs) = (asT ws', asT ls', asT cs')
        asT = T.pack . show

-- | Get counts for words, lines and characters from a TextBuffer.
-- This function makes use of GTK's line & character count functions
-- which perform caching and are thus likely faster than a manual approach.
count :: TextBuffer -> IO (Int, Int, Int)
count textBuffer = do
  lineCount <- fromIntegral <$> textBufferGetLineCount textBuffer
  charCount <- fromIntegral <$> textBufferGetCharCount textBuffer
  startIter <- textBufferGetStartIter textBuffer
  endIter   <- textBufferGetEndIter textBuffer
  contents  <- textBufferGetText textBuffer startIter endIter False
  return (length $ T.words contents, lineCount, charCount)
