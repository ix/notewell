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

nodeToPango :: Node -> Text
nodeToPango (Node _ STRONG children) =
  strong $ T.concat $ map nodeToPango children
nodeToPango (Node _ EMPH children) = emph $ T.concat $ map nodeToPango children
nodeToPango (Node _ (HEADING lvl) children) = heading lvl $ T.concat $ map nodeToPango children
nodeToPango (Node _ (CODE t) children) = code $ T.concat $ map nodeToPango children
nodeToPango (Node _ (TEXT t) _) = t
nodeToPango (Node _ _ children) = T.concat $ map nodeToPango children

-- | Apply emphasis to some text using Pango Markup.
emph :: Text -> Text
emph = P.span [P.Style P.ItalicStyle]

-- | Use a bold font weight for some text with Pango Markup.
strong :: Text -> Text
strong = P.span [P.Weight P.BoldWeight]

-- | Modify the size of some text according to its Header Level.
heading :: Level -> Text -> Text
heading level
  | level <= 1 = P.span [P.Size P.SizeXXLarge]
  | level == 2 = P.span [P.Size P.SizeXLarge]
  | level == 3 = P.span [P.Size P.SizeLarge]
  | level >= 4 = P.span [P.Size P.SizeMedium]

-- | Format some text as source code.
code :: Text -> Text
code = P.span [P.Family P.MonospaceFamily]

commonmarkToPango :: Text -> Text
commonmarkToPango = nodeToPango . commonmarkToNode [] []

bytes :: Text -> Int
bytes = BS.length . T.encodeUtf8
