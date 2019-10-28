{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

 Module : Main
 Description : Main module. 
 Copyright : Rose <rose@empty.town>
 License : BSD3
 Maintainer : rose@empty.town 
-}

module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Monad
import           Control.Concurrent.Async       ( async )
import qualified GI.GdkPixbuf                  as Gdk
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           Data.GI.Base.Overloading       ( IsDescendantOf )
import           GI.GLib.Functions              ( idleAdd )
import qualified GI.GLib.Constants             as GLib
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Notewell.Renderer
import           Notewell.Theming
import           Notewell.Theming.CSS
import           Notewell.Metrics
import           System.FilePath.Posix
import           System.Environment             ( getExecutablePath
                                                , getArgs
                                                )
import           Paths_notewell
import           Control.Monad.Trans.State
import           Data.Int                       ( Int32 )
import           Data.Either                    ( fromRight )
import System.IO.Unsafe

data Screen = Welcome | Editing

data Luggage = Luggage { screen     :: Screen
                       , buffer     :: Gtk.TextBuffer
                       , isDarkMode :: Bool }

type AppState a = State Luggage a

data Event = Closed                            -- ^ The window was closed.
           | OpenFileSelected (Maybe FilePath) -- ^ An open target was selected in the FileChooser.
           | NewClicked                        -- ^ The new document button was clicked.
           | OpenClicked                       -- ^ The open button was clicked.
           | SaveClicked                       -- ^ The save button was clicked.
           | SaveFileSelected (Maybe FilePath) -- ^ A save target was selected in the FileChooser.
           | Typed

-- | Constructs a FileFilter which pertains to Markdown documents.
markdownFileFilter :: IO Gtk.FileFilter
markdownFileFilter = do
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterSetName filt $ Just "Markdown"
  Gtk.fileFilterAddPattern filt "*.md"
  Gtk.fileFilterAddPattern filt "*.markdown"
  return filt

-- | Create a TextView widget from a given TextBuffer.
editor :: Gtk.TextBuffer -> Widget Event
editor textBuffer = widget
  Gtk.TextView
  [ afterCreated setBuffer
  , #wrapMode := Gtk.WrapModeWord
  , #margin := 10
  , classes ["editor"]
  ]
  where setBuffer tv = Gtk.textViewSetBuffer tv $ Just textBuffer

-- | A callback to be used with 'onM' to create a native file chooser dialog.
spawnFileDialog
  :: (Gtk.GObject w, IsDescendantOf Gtk.Widget w)
  => Gtk.FileChooserAction
  -> w
  -> IO (Maybe FilePath)
spawnFileDialog action invoker = do
  chooser <- Gtk.fileChooserNativeNew Nothing
                                      Gtk.noWindow
                                      action
                                      Nothing
                                      Nothing
  parentWindow <- Gtk.castTo Gtk.Window <$> Gtk.widgetGetToplevel invoker
  Gtk.nativeDialogSetModal chooser True
  Gtk.nativeDialogSetTransientFor chooser =<< parentWindow
  Gtk.fileChooserAddFilter chooser =<< markdownFileFilter
  statusCode <- Gtk.nativeDialogRun chooser
  if statusCode == accept
    then Gtk.fileChooserGetFilename chooser
    else return Nothing
  where accept = fromIntegral $ fromEnum Gtk.ResponseTypeAccept

-- | Render Markdown based on the content of a TextBuffer.
renderMarkdown :: Gtk.TextBuffer -> IO ()
renderMarkdown textBuffer = clearTags textBuffer >> formatBuffer textBuffer

-- | Remove all TextTags from a buffer.
clearTags :: Gtk.TextBuffer -> IO ()
clearTags textBuffer = do
  s <- Gtk.textBufferGetStartIter textBuffer
  e <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferRemoveAllTags textBuffer s e

-- | Retrieve the contents of a TextBuffer.
getBufferContents :: Gtk.TextBuffer -> IO Text
getBufferContents textBuffer = do
  s <- Gtk.textBufferGetStartIter textBuffer
  e <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferGetText textBuffer s e True

-- | Place a widget inside a BoxChild and allow it to expand.
expandableChild :: Widget a -> BoxChild a
expandableChild =
  BoxChild defaultBoxChildProperties { expand = True, fill = True }

-- | Simply a shorthand for the toolbar component.
toolbar :: AppState (BoxChild Event)
toolbar = do
  textBuffer <- gets buffer
  return $ container
    Gtk.Box
    [#orientation := Gtk.OrientationHorizontal, classes ["toolbar"]]
    [ widget
      Gtk.Button
      [ onM #clicked $ \button -> do
        filename <- spawnFileDialog Gtk.FileChooserActionSave button
        return $ SaveFileSelected filename
      , #label := "Save"
      ]
    , widget Gtk.Label [#label := (T.pack . show $ count textBuffer)]
    ]

-- | Used as a callback with afterCreated to set the icon of a ToolButton.
setIcon :: Int32 -> FilePath -> Gtk.ToolButton -> IO ()
setIcon size fp tb = do
  path  <- takeDirectory <$> getExecutablePath
  icon  <- Gdk.pixbufNewFromFileAtSize (path </> fp) size size
  image <- Gtk.imageNewFromPixbuf $ Just icon
  Gtk.toolButtonSetIconWidget tb $ Just image

-- | Get the path to an icon, taking into account the theme.
getIconPath :: FilePath -> AppState FilePath
getIconPath filename = do
  mode <- gets isDarkMode
  return
    $   "themes"
    </> "icons"
    </> (if mode then "dark" else "light")
    </> filename

view' :: AppState (AppView Gtk.Window Event)
view' = do
  s                <- gets screen
  b                <- gets buffer
  iconNewFile      <- getIconPath "new-file.svg"
  iconFolderOpened <- getIconPath "folder-opened.svg"
  toolbar' <- toolbar
  return
    $ bin
        Gtk.Window
        [ #title := "Notewell"
        , on #deleteEvent (const (True, Closed))
        , #widthRequest := 480
        , #heightRequest := 300
        ]
    $ case s of
        Welcome ->
          container Gtk.Box [#orientation := Gtk.OrientationHorizontal]
            $ [ expandableChild $ widget
                Gtk.ToolButton
                [ afterCreated $ setIcon 64 iconNewFile
                , on #clicked NewClicked
                , classes ["intro"]
                ]
              , expandableChild $ widget
                Gtk.ToolButton
                [ afterCreated $ setIcon 64 iconFolderOpened
                , onM #clicked $ \button -> do
                  filename <- spawnFileDialog Gtk.FileChooserActionOpen button
                  return $ OpenFileSelected filename
                , classes ["intro"]
                ]
              ]
        Editing ->
          container Gtk.Box [#orientation := Gtk.OrientationVertical]
            $ [ expandableChild $ bin Gtk.ScrolledWindow [] $ editor b
              , toolbar' 
              ]

update' :: Luggage -> Event -> Transition Luggage Event
update' s NewClicked = Transition s { screen = Editing } $ return Nothing
update' s (SaveFileSelected (Just file)) =
  Transition s { screen = Editing } $ do
    T.writeFile file =<< (getBufferContents $ buffer s)
    return Nothing
update' s (SaveFileSelected Nothing) =
  Transition s { screen = Editing } $ return Nothing
update' s (OpenFileSelected (Just file)) =
  Transition s { screen = Editing } $ do
  -- Wait on redraw operations before it's safe to modify the buffer.
    void $ idleAdd GLib.PRIORITY_HIGH_IDLE $ do
      contents <- T.readFile file
      Gtk.textBufferSetText (buffer s) contents $ fromIntegral $ bytes contents
      renderMarkdown (buffer s)
      return False
    return $ Nothing
update' s (OpenFileSelected Nothing) =
  Transition s { screen = Editing } $ return Nothing
update' _ _ = Exit

main :: IO ()
main = getArgs >>= parseOpts

parseOpts :: [String] -> IO ()
parseOpts ("--theme" : themeName : _) = do
  path  <- getDataFileName ("themes" </> themeName <.> "json")
  theme <- fromRight defaultTheme <$> readTheme path
  startWithTheme theme
parseOpts _ = do
  path  <- getDataFileName ("themes" </> "giorno" <.> "json")
  theme <- fromRight defaultTheme <$> readTheme path
  startWithTheme theme

startWithTheme :: Theme -> IO ()
startWithTheme theme = do
  void $ Gtk.init Nothing

  provider  <- Gtk.cssProviderNew
  settings  <- Gtk.settingsGetDefault
  gtkScreen <- maybe (fail "No screen?") return =<< Gdk.screenGetDefault
  buff      <- Gtk.textBufferNew . Just =<< markdownTextTagTable theme

  void $ Gtk.on buff #endUserAction (renderMarkdown buff)

  let app = App { view         = evalState view'
                , update       = update'
                , inputs       = []
                , initialState = Luggage Welcome buff $ isDark theme
                }

  case settings of
    Just settings' -> do
      Gtk.setSettingsGtkCursorBlink settings' False
      when (isDark theme)
        $ Gtk.setSettingsGtkApplicationPreferDarkTheme settings' True
    Nothing -> return ()

  Gtk.cssProviderLoadFromData provider $ buildCSS theme
  Gtk.styleContextAddProviderForScreen
    gtkScreen
    provider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
