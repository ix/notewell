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
import qualified Bene.Pango                    as P

commonmarkToPango :: Node -> Text
commonmarkToPango (Node _ STRONG children) =
    strong $ T.concat $ map commonmarkToPango children
commonmarkToPango (Node _ EMPH children) =
    emph $ T.concat $ map commonmarkToPango children
commonmarkToPango (Node _ (TEXT t) _) = t

-- | Apply emphasis to some text using Pango Markup.
emph :: Text -> Text
emph = P.span [P.Style P.ItalicStyle]

strong :: Text -> Text
strong = P.span [P.Weight P.BoldWeight]

parse :: Text -> Node
parse = commonmarkToNode [] []

bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
