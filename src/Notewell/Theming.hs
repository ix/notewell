{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Notewell.Theming where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics
import           GI.Pango.Enums
import           GI.Gtk.Enums
import           Data.Int                       ( Int32 )

data TagProperties = TagProperties { color  :: Maybe Text
                                   , font   :: Maybe Text
                                   , scale  :: Maybe Double
                                   , style  :: Maybe Style
                                   , weight :: Maybe Weight
                                   , indent :: Maybe Int32
                                   , strike :: Maybe Bool
                                   , justification :: Maybe Justification }
  deriving (Generic, Show, Eq)

data Theme = Theme { background    :: Text
                   , foreground    :: Text
                   , accent        :: Text
                   , bodyFont      :: Text
                   , isDark        :: Bool
                   , emph          :: TagProperties
                   , strong        :: TagProperties
                   , code          :: TagProperties
                   , codeBlock     :: TagProperties
                   , strikethrough :: TagProperties
                   , thematicBreak :: TagProperties
                   , list          :: TagProperties
                   , blockquote    :: TagProperties
                   , heading       :: TagProperties }
  deriving (Generic, Show, Eq)

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

instance FromJSON TagProperties
instance ToJSON TagProperties

instance ToJSON Theme
instance FromJSON Theme

readTheme :: FilePath -> IO (Either String Theme)
readTheme = eitherDecodeFileStrict

defaultTheme :: Theme
defaultTheme = Theme { foreground    = "black"
                     , background    = "white"
                     , accent        = "red"
                     , bodyFont      = "Sans"
                     , isDark        = False
                     , emph          = blank
                     , strong        = blank
                     , code          = blank
                     , codeBlock     = blank
                     , strikethrough = blank
                     , thematicBreak = blank
                     , list          = blank
                     , blockquote    = blank
                     , heading       = blank
                     }
 where
  blank = TagProperties Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
