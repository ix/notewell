{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{- |
 Module : Main
 Description : Main module.
 Copyright : Rose <rose@empty.town>
 License : BSD3
 Maintainer : rose@empty.town
-}

module Main where

import Control.Arrow
import Control.Concurrent.Async      (async)
import Control.Monad                 (forM_)
import Control.Monad.Reader          (MonadReader, Reader, asks, runReader, void)
import Data.Either                   (fromRight)
import Data.GI.Base.Overloading      (IsDescendantOf)
import Data.Int                      (Int32)
import Data.Text                     (Text)
import GI.GLib.Functions             (idleAdd)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Environment            (getArgs, getExecutablePath)
import System.FilePath.Posix         (takeDirectory, (<.>), (</>))

import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import qualified GI.Gdk            as Gdk
import qualified GI.GdkPixbuf      as Gdk
import qualified GI.GLib.Constants as GLib
import qualified GI.Gtk            as Gtk
import qualified Notewell.Metrics  as M

import Notewell.Editor
import Notewell.Events
import Notewell.Helpers
import Notewell.Renderer
import Notewell.Theming
import Notewell.Theming.CSS

data Screen = Welcome | Editing

data Properties = Properties { screen       :: Screen
                             , editorParams :: EditorParams
                             , metrics      :: M.Metrics }

newtype EditorEnv a = EditorEnv { unEditorEnv :: Reader Properties a }
  deriving (Functor, Applicative, Monad, MonadReader Properties)

-- | Constructs a FileFilter which pertains to Markdown documents.
markdownFileFilter :: IO Gtk.FileFilter
markdownFileFilter = do
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterSetName filt $ Just "Markdown"
  Gtk.fileFilterAddPattern filt "*.md"
  Gtk.fileFilterAddPattern filt "*.markdown"
  pure filt

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
  if statusCode == accept then Gtk.fileChooserGetFilename chooser else pure Nothing
  where accept = fromIntegral $ fromEnum Gtk.ResponseTypeAccept

-- | Render Markdown based on the content of a TextBuffer.
renderMarkdown :: Gtk.TextBuffer -> IO ()
renderMarkdown = void <$> runKleisli (Kleisli removeAllTags &&& Kleisli applyAllTags)

-- | Place a widget inside a BoxChild and allow it to expand.
expandableChild :: Widget a -> BoxChild a
expandableChild = BoxChild defaultBoxChildProperties { expand = True, fill = True }

-- | Simply a shorthand for the toolbar component.
toolbar :: EditorEnv (BoxChild Event)
toolbar = do
  textBuffer   <- asks (editorBuffer . editorParams)
  metrics'     <- asks metrics
  currentTheme <- asks (editorTheme . editorParams)
  pure $ container
    Gtk.Box
    [#orientation := Gtk.OrientationHorizontal, classes ["toolbar"]]
    [ widget
      Gtk.Button
      [ onM #clicked $ \button -> do
        filename <- spawnFileDialog Gtk.FileChooserActionSave button
        pure $ SaveFileSelected filename
      , #label := "Save"
      ]
    , expandableChild $ widget Gtk.Label [#label := M.formatCounts metrics', #halign := Gtk.AlignEnd]
    , widget Gtk.Switch [ #state := isDark currentTheme,
                          onM #stateSet $ \state -> pure $ do
                            table <- #getTagTable textBuffer
                            newTheme <- getDefaultTheme $ fromBool state
                            switchTagTable currentTheme newTheme table
                            pure (False, ToggleTheme $ fromBool state)
                        ]
    ]

-- | Attempts to mutate a TextTagTable to one built from a Theme.
switchTagTable :: Theme -> Theme -> Gtk.TextTagTable -> IO ()
switchTagTable oldTheme newTheme table = do
  clearTable table oldTheme
  populateTable table newTheme

-- | Used as a callback with afterCreated to set the icon of a ToolButton.
setIcon :: Int32 -> FilePath -> Gtk.ToolButton -> IO ()
setIcon size fp tb = do
  path  <- takeDirectory <$> getExecutablePath
  icon  <- Gdk.pixbufNewFromFileAtSize (path </> fp) size size
  image <- Gtk.imageNewFromPixbuf $ Just icon
  Gtk.toolButtonSetIconWidget tb $ Just image

-- | Get the path to an icon, taking into account the theme.
getIconPath :: FilePath -> EditorEnv FilePath
getIconPath filename = do
  mode <- isDark <$> asks (editorTheme . editorParams)
  pure $ "themes" </> "icons" </> (if mode then "dark" else "light") </> filename

-- | Builds a declarative view from the EditorEnv.
view' :: EditorEnv (AppView Gtk.Window Event)
view' = do
  s                <- asks screen
  params           <- asks editorParams
  iconNewFile      <- getIconPath "new-file.svg"
  iconFolderOpened <- getIconPath "folder-opened.svg"
  toolbar'         <- toolbar
  pure
    $ bin Gtk.Window
          [#title := "Notewell", on #deleteEvent (const (True, Closed)), #widthRequest := 480, #heightRequest := 300]
    $ case s of
        Welcome ->
          container Gtk.Box [#orientation := Gtk.OrientationHorizontal]
          [ expandableChild $ widget
            Gtk.ToolButton
            [afterCreated $ setIcon 64 iconNewFile, on #clicked NewClicked, classes ["intro"]]
          , expandableChild $ widget
            Gtk.ToolButton
            [ afterCreated $ setIcon 64 iconFolderOpened
            , onM #clicked $ \button -> do
                filename <- spawnFileDialog Gtk.FileChooserActionOpen button
                pure $ OpenFileSelected filename
            , classes ["intro"]
            ]
          ]
        Editing ->
          container Gtk.Box [#orientation := Gtk.OrientationVertical]
          [ expandableChild $ bin Gtk.ScrolledWindow [] $ editorHelper params, toolbar' ]

-- | Perform state transitions.
update' :: Properties -> Event -> Transition Properties Event
update' s NewClicked                     = Transition s { screen = Editing } $ pure Nothing
update' s (SaveFileSelected (Just file)) = Transition s { screen = Editing } $ do
  contents <- let buffer = (editorBuffer . editorParams) s in Gtk.get buffer #text
  whenM contents $ \text -> T.writeFile file text
  pure $ Just Render
update' s (SaveFileSelected Nothing    ) = Transition s { screen = Editing } $ pure Nothing
update' s (OpenFileSelected (Just file)) = Transition s { screen = Editing } $ do
  let buffer = (editorBuffer . editorParams) s

  -- Wait on redraw operations before it's safe to modify the buffer.
  void $ idleAdd GLib.PRIORITY_HIGH_IDLE $ do
    contents <- T.readFile file
    Gtk.set buffer [#text Gtk.:= contents]
    pure False

  newCounts <- M.count buffer
  pure $ Just $ UpdateMetrics $ M.Metrics { M.counts = newCounts }
update' s (OpenFileSelected Nothing) = Transition s { screen = Editing } $ pure Nothing
update' s (UpdateMetrics    m      ) = Transition s { metrics = m } $ pure $ Just Render
update' s Render                     = Transition s $ do
  void $ idleAdd GLib.PRIORITY_HIGH_IDLE $ do
    renderMarkdown $ (editorBuffer . editorParams) s
    pure False
  pure Nothing
update' s (ToggleTheme t) = Transition s $ setDefaultTheme t >> pure (Just Render)
update' _ _               = Exit

-- | Call a monadic action with GTK settings, only if available.
withSettings :: (Gtk.Settings -> IO ()) -> IO ()
withSettings action = Gtk.settingsGetDefault >>= flip whenM action

-- | Set the default light or dark theme according to a ThemeType.
setDefaultTheme :: ThemeType -> IO ()
setDefaultTheme Light = withSettings <$> setTheme =<< getDefaultTheme Light
setDefaultTheme Dark  = withSettings <$> setTheme =<< getDefaultTheme Dark

-- | Set a Theme application-wide.
setTheme :: Theme -> Gtk.Settings -> IO ()
setTheme theme settings = do
  provider    <- Gtk.cssProviderNew
  maybeScreen <- maybe Nothing Just <$> Gdk.screenGetDefault
  whenM maybeScreen $ \screen ->
    void . idleAdd GLib.PRIORITY_LOW $ do
      Gtk.cssProviderLoadFromData provider $ buildCSS theme
      Gtk.styleContextAddProviderForScreen screen provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
      Gtk.setSettingsGtkApplicationPreferDarkTheme settings $ isDark theme
      pure False

main :: IO ()
main = getArgs >>= parseOpts

parseOpts :: [String] -> IO ()
parseOpts ("--theme" : themeName : _) = do
  let path = "themes" </> themeName <.> "json"
  theme <- fromRight defaultTheme <$> readTheme path
  startWithTheme theme
parseOpts _ = do
  let path = "themes" </> "giorno" <.> "json"
  theme <- fromRight defaultTheme <$> readTheme path
  startWithTheme theme

startWithTheme :: Theme -> IO ()
startWithTheme theme = do
  void $ Gtk.init Nothing

  textTagTable <- markdownTextTagTable theme
  globalBuffer <- Gtk.new Gtk.TextBuffer [#tagTable Gtk.:= textTagTable]

  let app = App
        { view         = runReader $ unEditorEnv view'
        , update       = update'
        , inputs       = []
        , initialState = Properties Welcome (EditorParams { editorTheme = theme, editorBuffer = globalBuffer }) M.empty
        }

  withSettings $ \settings -> do
    setTheme theme settings
    Gtk.setSettingsGtkCursorBlink settings False

  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
