{- |
 Module : Notewell.Metrics
 Description : Analysis functions for GTK TextBuffer. 
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

module Notewell.Metrics where

import           GI.Gtk
import qualified Data.Text                     as T
import System.IO.Unsafe

-- | Get counts for words, lines and characters from a TextBuffer.
-- This function makes use of GTK's line & character count functions
-- which perform caching and are thus likely faster than a manual approach.
count :: TextBuffer -> (Int, Int, Int)
count textBuffer = unsafePerformIO $ do
  lineCount <- fromIntegral <$> textBufferGetLineCount textBuffer
  charCount <- fromIntegral <$> textBufferGetCharCount textBuffer
  startIter <- textBufferGetStartIter textBuffer
  endIter   <- textBufferGetEndIter textBuffer
  contents  <- textBufferGetText textBuffer startIter endIter False
  return (length $ T.words contents, lineCount, charCount)
