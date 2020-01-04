{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
  Module : Notewell.Theming
  Description : An Aeson wrapper around GTK TextTags.
  Copyright : Rose <rose@empty.town> & Max
  License : BSD3
  Maintainer : rose@empty.town
-}

module Notewell.Theming where

import Control.Applicative ((<|>))
import Data.Aeson          (FromJSON, ToJSON, Value (..),
                            eitherDecodeFileStrict, parseJSON, toJSON,
                            withObject, (.:))
import Data.Int            (Int32)
import Data.Text           (Text)
import GHC.Generics        (Generic)
import GI.Gtk.Enums
import GI.Pango.Enums

import qualified Data.HashMap.Strict as HM

data TagProperty = Color Text
                 | Font Text
                 | Scale Double
                 | Style Style
                 | Weight Weight
                 | Indent Int32
                 | Strike Bool
                 | Justification Justification
                 | Underline Underline
  deriving (Generic, Eq)

type TagProperties = [TagProperty]

data Theme = Theme { background   :: Text
                   , foreground   :: Text
                   , toolbarColor :: Text
                   , borderColor  :: Text
                   , accent       :: Text
                   , bodyFont     :: Text
                   , isDark       :: Bool
                   , elements     :: !(HM.HashMap Text TagProperties) }
  deriving (Generic, Eq)

instance FromJSON TagProperty where
  parseJSON = withObject "TagProperty" $ \v ->
    (Color         <$> v .: "color")         <|>
    (Font          <$> v .: "font")          <|>
    (Scale         <$> v .: "scale")         <|>
    (Style         <$> v .: "style")         <|>
    (Weight        <$> v .: "weight")        <|>
    (Indent        <$> v .: "indent")        <|>
    (Strike        <$> v .: "strikethrough") <|>
    (Justification <$> v .: "justification") <|>
    (Underline     <$> v .: "underline")

instance ToJSON TagProperty

instance ToJSON Theme
instance FromJSON Theme

instance FromJSON Underline where
  parseJSON (String t) = return $ case t of
    "none"   -> UnderlineNone
    "single" -> UnderlineSingle
    "double" -> UnderlineDouble
    "low"    -> UnderlineLow
    "wavy"   -> UnderlineError
    _        -> UnderlineNone

instance ToJSON Underline where
  toJSON UnderlineNone   = "none"
  toJSON UnderlineSingle = "single"
  toJSON UnderlineDouble = "double"
  toJSON UnderlineLow    = "low"
  toJSON UnderlineError  = "wavy"
  toJSON _               = "none"

instance FromJSON Justification where
  parseJSON (String t) = return $ case t of
    "left"   -> JustificationLeft
    "right"  -> JustificationRight
    "center" -> JustificationCenter
    "fill"   -> JustificationFill
    _        -> JustificationLeft

instance ToJSON Justification where
  toJSON JustificationLeft   = "left"
  toJSON JustificationRight  = "right"
  toJSON JustificationCenter = "center"
  toJSON JustificationFill   = "fill"

instance FromJSON Weight where
  parseJSON (String t) = return $ case t of
    "thin"       -> WeightThin
    "ultralight" -> WeightUltralight
    "light"      -> WeightLight
    "semilight"  -> WeightSemilight
    "book"       -> WeightBook
    "normal"     -> WeightNormal
    "medium"     -> WeightMedium
    "semibold"   -> WeightSemibold
    "bold"       -> WeightBold
    "ultrabold"  -> WeightUltrabold
    "heavy"      -> WeightHeavy
    "ultraheavy" -> WeightUltraheavy
    _            -> WeightNormal

instance ToJSON Weight where
  toJSON WeightThin       = String "thin"
  toJSON WeightUltralight = String "ultralight"
  toJSON WeightLight      = String "light"
  toJSON WeightSemilight  = String "semilight"
  toJSON WeightBook       = String "book"
  toJSON WeightNormal     = String "normal"
  toJSON WeightMedium     = String "medium"
  toJSON WeightSemibold   = String "semibold"
  toJSON WeightBold       = String "bold"
  toJSON WeightUltrabold  = String "ultrabold"
  toJSON WeightHeavy      = String "heavy"
  toJSON WeightUltraheavy = String "ultraheavy"

instance FromJSON Style where
  parseJSON (String t) = return $ case t of
    "italic"  -> StyleItalic
    "oblique" -> StyleOblique
    _         -> StyleNormal

instance ToJSON Style where
  toJSON s = String $ case s of
    StyleItalic  -> "italic"
    StyleOblique -> "oblique"
    StyleNormal  -> "normal"

readTheme :: FilePath -> IO (Either String Theme)
readTheme = eitherDecodeFileStrict

defaultTheme :: Theme
defaultTheme = Theme { foreground   = "black"
                     , background   = "white"
                     , toolbarColor = "grey"
                     , borderColor  = "darkgrey"
                     , accent       = "red"
                     , bodyFont     = "Sans"
                     , isDark       = False
                     , elements     = HM.empty
                     }

