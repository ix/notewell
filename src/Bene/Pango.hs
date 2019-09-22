{-# LANGUAGE OverloadedStrings #-}
{- |
 Module : Bene.Pango
 Description : A wrapper for a subset of the Pango Markup Language. 
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}
module Bene.Pango where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

data FontFamily = NormalFamily | SansFamily | SerifFamily | MonospaceFamily

instance Show FontFamily where
  show NormalFamily    = "normal"
  show SansFamily      = "sans"
  show SerifFamily     = "serif"
  show MonospaceFamily = "monospace"

data FontSize =
    SizeXXSmall
  | SizeXSmall
  | SizeSmall
  | SizeMedium
  | SizeLarge
  | SizeXLarge
  | SizeXXLarge

instance Show FontSize where
  show SizeXXSmall = "xx-small"
  show SizeXSmall  = "x-small"
  show SizeSmall   = "small"
  show SizeMedium  = "medium"
  show SizeLarge   = "large"
  show SizeXLarge  = "x-large"
  show SizeXXLarge = "xx-large"

data FontStyle = NormalStyle | ObliqueStyle | ItalicStyle

instance Show FontStyle where
  show NormalStyle  = "normal"
  show ObliqueStyle = "oblique"
  show ItalicStyle  = "italic"

data UnderlineStyle =
    SingleUnderline
  | DoubleUnderline
  | LowUnderline
  | NoUnderline

instance Show UnderlineStyle where
  show SingleUnderline = "single"
  show DoubleUnderline = "double"
  show LowUnderline    = "low"
  show NoUnderline     = "none"

data FontWeight =
    UltraLightWeight
  | LightWeight
  | NormalWeight
  | BoldWeight
  | UltraBoldWeight
  | HeavyWeight

instance Show FontWeight where
  show UltraLightWeight = "ultralight"
  show LightWeight      = "light"
  show NormalWeight     = "normal"
  show BoldWeight       = "bold"
  show UltraBoldWeight  = "ultrabold"
  show HeavyWeight      = "heavy"

data Attribute =
    FontDesc String
  | Family FontFamily
  | Size FontSize
  | Style FontStyle
  | Weight FontWeight
  | Foreground String
  | Background String
  | Underline UnderlineStyle
  | Strikethrough

instance Show Attribute where
  show (FontDesc   t) = "font_desc=\"" ++ t ++ "\""
  show (Family     f) = "font_family=\"" ++ show f ++ "\""
  show (Size       s) = "size=\"" ++ show s ++ "\""
  show (Style      s) = "style=\"" ++ show s ++ "\""
  show (Weight     w) = "weight=\"" ++ show w ++ "\""
  show (Foreground s) = "foreground=\"" ++ s ++ "\""
  show (Background s) = "background=\"" ++ s ++ "\""
  show (Underline  u) = "underline=\"" ++ show u ++ "\""
  show Strikethrough  = "strikethrough=\"true\""

-- | Wraps a given Text value in a span tag with the list of Attributes applied.
span :: [Attribute] -> Text -> Text
span []         body = T.concat ["<span>", body, "</span>"]
span attributes body = T.concat [opening, body, closing]
 where
  opening = T.concat ["<span ", attrs, ">"]
  closing = "</span>"
  attrs   = T.pack $ unwords $ map show attributes
