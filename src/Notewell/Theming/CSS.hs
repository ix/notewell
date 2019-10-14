{- |
 Module : Notewell.Theming.CSS
 Description : Used by Notewell for constructing some application CSS.
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

{-# LANGUAGE OverloadedStrings #-}

module Notewell.Theming.CSS where

import           Notewell.Theming
import           Prelude                 hiding ( (++) )
import Data.Text.Encoding
import Data.ByteString.Char8

buildCSS :: Theme -> ByteString
buildCSS theme =
  "\
\@define-color bg_color "
    ++ bgColor
    ++ ";\
\@define-color accent_color "
    ++ acColor
    ++ ";\
\@define-color fg_color "
    ++ fgColor
    ++ ";\
\.intro button {\
\    border: none;\
\    border-radius: 0px;\
\    margin: 0;\
\    padding: 0;\
\    background: @bg_color;\
\    font-size: 1.2em;\
\    box-shadow: none;\
\}\
\.intro button:active {\
\    background: @accent_color;\
\}\
\decoration {\
\    background: @bg_color;\
\}\
\scrolledwindow { background: @bg_color; }\
\.editor {\
\    font-family: "
    ++ bf
    ++ ";\
\    font-size: 1.25em;\
\    background: @bg_color;\
\    caret-color: @accent_color;\
\    -GtkWidget-cursor-aspect-ratio: 0.05;\
\    animation: blink 0.5s ease-in-out alternate infinite;\
\}\
\@keyframes blink {\
\    0% { caret-color: @accent_color; }\
\    100% { caret-color: rgba(0, 0, 0, 0); }\
\}\
\.editor text {\
\    background: @bg_color;\
\    color: @fg_color;\
\}\
\.editor text selection {\
\    background: @accent_color;\
\    color: @bg_color;\
\}\
\.toolbar { background: #eee; border-top: 1px solid #ccc; }\
\.toolbar button { \
\    background: #eee;\
\    border: none;\
\    box-shadow: none;\
\    text-shadow: none;\
\    color: #333;\
\    border-radius: 0px;\
\    padding: 0em 1em 0em 1em;\
\}\
\.toolbar button:hover {\
\    background: #ddd;\
\    color: @accent_color;\
\    transition: color 0.5s;\
\}\
\.toolbar button:active { background: #ccc; }"
 where
  (++)    = append
  bgColor = encodeUtf8 $ background theme
  fgColor = encodeUtf8 $ foreground theme
  acColor = encodeUtf8 $ accent theme
  bf      = encodeUtf8 $ bodyFont theme
