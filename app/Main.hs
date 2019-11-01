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
import qualified Notewell.Metrics              as M
import           Notewell.Editor
import           Notewell.Events                ( Event(..) )
import           Notewell.Helpers               ( whenM )
import           System.FilePath.Posix
import           System.Environment             ( getExecutablePath
                                                , getArgs
                                                )
import           Paths_notewell
import           Control.Arrow
import           Control.Monad.Trans.State.Strict
import           Data.Int                       ( Int32 )
import           Data.Either                    ( fromRight )

data Screen = Welcome | Editing

data Luggage = Luggage { screen         :: Screen
                       , editorParams   :: EditorParams
                       , metrics        :: M.Metrics }

type AppState a = State Luggage a

-- | Constructs a FileFilter which pertains to Markdown documents.
markdownFileFilter :: IO Gtk.FileFilter
markdownFileFilter = do
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterSetName filt $ Just "Markdown"
  Gtk.fileFilterAddPattern filt "*.md"
  Gtk.fileFilterAddPattern filt "*.markdown"
  return filt

-- | Creates a markdown editor widget with some default options.
editorHelper :: EditorParams -> Widget Event
editorHelper = markdownEditor [#wrapMode := Gtk.WrapModeWord, #margin := 10, classes ["editor"]]

-- | A callback to be used with 'onM' to create a native file chooser dialog.
spawnFileDialog :: (Gtk.GObject w, IsDescendantOf Gtk.Widget w) => Gtk.FileChooserAction -> w -> IO (Maybe FilePath)
spawnFileDialog action invoker = do
  chooser      <- Gtk.fileChooserNativeNew Nothing Gtk.noWindow action Nothing Nothing
  parentWindow <- Gtk.castTo Gtk.Window <$> Gtk.widgetGetToplevel invoker
  Gtk.nativeDialogSetModal chooser True
  Gtk.nativeDialogSetTransientFor chooser =<< parentWindow
  Gtk.fileChooserAddFilter chooser =<< markdownFileFilter
  statusCode <- Gtk.nativeDialogRun chooser
  if statusCode == accept then Gtk.fileChooserGetFilename chooser else return Nothing
  where accept = fromIntegral $ fromEnum Gtk.ResponseTypeAccept

-- | Render Markdown based on the content of a TextBuffer.
renderMarkdown :: Gtk.TextBuffer -> IO ()
renderMarkdown = fmap void $ runKleisli (Kleisli removeAllTags &&& Kleisli applyAllTags)

-- | Place a widget inside a BoxChild and allow it to expand.
expandableChild :: Widget a -> BoxChild a
expandableChild = BoxChild defaultBoxChildProperties { expand = True, fill = True }

-- | Simply a shorthand for the toolbar component.
toolbar :: AppState (BoxChild Event)
toolbar = do
  textBuffer <- gets (editorBuffer . editorParams)
  metrics'   <- gets metrics
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
    , expandableChild $ widget Gtk.Label [#label := M.formatCounts metrics', #halign := Gtk.AlignEnd]
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
  mode <- isDark <$> gets (editorTheme . editorParams)
  return $ "themes" </> "icons" </> (if mode then "dark" else "light") </> filename

-- | Builds a declarative view from the AppState.
view' :: AppState (AppView Gtk.Window Event)
view' = do
  s                <- gets screen
  params           <- gets editorParams
  iconNewFile      <- getIconPath "new-file.svg"
  iconFolderOpened <- getIconPath "folder-opened.svg"
  toolbar'         <- toolbar
  return
    $ bin Gtk.Window
          [#title := "Notewell", on #deleteEvent (const (True, Closed)), #widthRequest := 480, #heightRequest := 300]
    $ case s of
        Welcome ->
          container Gtk.Box [#orientation := Gtk.OrientationHorizontal]
            $ [ expandableChild $ widget
                Gtk.ToolButton
                [afterCreated $ setIcon 64 iconNewFile, on #clicked NewClicked, classes ["intro"]]
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
            $ [expandableChild $ bin Gtk.ScrolledWindow [] $ editorHelper params, toolbar']

-- | Perform state transitions.
update' :: Luggage -> Event -> Transition Luggage Event
update' s NewClicked                     = Transition s { screen = Editing } $ return Nothing
update' s (SaveFileSelected (Just file)) = Transition s { screen = Editing } $ do
  contents <- let buffer = (editorBuffer . editorParams) s in Gtk.get buffer #text
  whenM contents $ \text -> T.writeFile file text
  return $ Just Render
update' s (SaveFileSelected Nothing    ) = Transition s { screen = Editing } $ return Nothing
update' s (OpenFileSelected (Just file)) = Transition s { screen = Editing } $ do
  let buffer = (editorBuffer . editorParams) s 
  
  -- Wait on redraw operations before it's safe to modify the buffer.
  void $ idleAdd GLib.PRIORITY_HIGH_IDLE $ do
    contents <- T.readFile file
    Gtk.set buffer [#text Gtk.:= contents]
    return False

  newCounts <- M.count buffer
  return $ Just $ UpdateMetrics $ M.Metrics { M.counts = newCounts }
update' s (OpenFileSelected Nothing) = Transition s { screen = Editing } $ return Nothing
update' s (UpdateMetrics    m)       = Transition s { metrics = m } $ return $ Just Render
update' s Render                     = Transition s $ do
  void $ idleAdd GLib.PRIORITY_HIGH_IDLE $ do
    renderMarkdown $ (editorBuffer . editorParams) s
    return False
  return Nothing
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

  provider     <- Gtk.cssProviderNew
  settings     <- Gtk.settingsGetDefault
  gtkScreen    <- maybe (fail "No screen?") return =<< Gdk.screenGetDefault
  textTagTable <- markdownTextTagTable theme
  globalBuffer <- Gtk.new Gtk.TextBuffer [#tagTable Gtk.:= textTagTable]

  let
    app = App
      { view         = evalState view'
      , update       = update'
      , inputs       = []
      , initialState = Luggage Welcome (EditorParams { editorTheme = theme, editorBuffer = globalBuffer }) M.empty
      }

  case settings of
    Just settings' -> do
      Gtk.setSettingsGtkCursorBlink settings' False
      when (isDark theme) $ Gtk.setSettingsGtkApplicationPreferDarkTheme settings' True
    Nothing -> return ()

  Gtk.cssProviderLoadFromData provider $ buildCSS theme
  Gtk.styleContextAddProviderForScreen gtkScreen provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
