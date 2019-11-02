{- |
 Module : Notewell.Theming.CSS
 Description : Used by Notewell for constructing some application CSS.
 Copyright : Rose <rose@empty.town> & Max
 License : BSD3
 Maintainer : rose@empty.town
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Notewell.Theming.CSS where

import           Notewell.Theming
import           Prelude                 hiding ( (++) )
import           Data.Text.Encoding
import           Data.ByteString.Char8
import           Text.Shakespeare.Text

buildCSS :: Theme -> ByteString
buildCSS theme = encodeUtf8 $ [st|
  @define-color bg_color #{bgColor};
  @define-color accent_color #{acColor};
  @define-color fg_color #{fgColor};
  .intro button {
      border: none;
      border-radius: 0px;
      margin: 0;
      padding: 0;
      background: @bg_color;
      font-size: 1.2em;
      box-shadow: none;
  }
  .intro button:active {
      background: @accent_color;
  }
  decoration {
      background: @bg_color;
  }
  scrolledwindow { background: @bg_color; }
  .editor {
      font-family: #{bf};
      font-size: 1.25em;
      background: @bg_color;
      caret-color: @accent_color;
      -GtkWidget-cursor-aspect-ratio: 0.05;
      animation: blink 0.5s ease-in-out alternate infinite;
  }
  @keyframes blink {
      0% { caret-color: @accent_color; }
      100% { caret-color: rgba(0, 0, 0, 0); }
  }
  .editor text {
      background: @bg_color;
      color: @fg_color;
  }
  .editor text selection {
      background: @accent_color;
      color: @bg_color;
  }
  .toolbar { 
      background: #{toolbar};
      border-top: 1px solid #{border};
  }
  .toolbar button {
      background: #{toolbar};
      border: none;
      box-shadow: none;
      text-shadow: none;
      color: #{fgColor};
      border-radius: 0px;
      padding: 0em 0.5em 0em 0.5em;
  }
  .toolbar button:hover {
      background: #{border};
      color: @accent_color;
      transition: color 0.5s;
  }
  .toolbar button:active { background: #{border}; }
  .toolbar label { padding: 0em 1em 0em 1em; }
  switch { margin: 0.3rem; }
 |]
 where
  bgColor = background theme
  fgColor = foreground theme
  acColor = accent theme
  toolbar = toolbarColor theme
  border  = borderColor theme
  bf      = bodyFont theme
