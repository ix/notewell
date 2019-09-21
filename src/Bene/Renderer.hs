{-# LANGUAGE OverloadedStrings #-}
{- |
 Module : Bene.Renderer
 Description : Renders CMark node trees as GTK widgets.
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

module Bene.Renderer where

import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text.Encoding as T

import CMarkGFM
import qualified Data.Text as T
import qualified Bene.Pango as P

pangoify :: Node -> Text
pangoify (Node _ (HEADING lvl) children) = P.span [P.Size P.SizeXXLarge] $ T.concat $ map pangoify children
pangoify (Node _ (TEXT text) _) = text

parse :: Text -> Node
parse = commonmarkToNode [] []

bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
