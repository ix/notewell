{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import           Control.Monad
import           Control.Applicative
import           Control.Concurrent.Async       ( async )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import qualified Data.ByteString.Char8         as BS
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           System.IO.Unsafe
import           Bene.Renderer
import           Paths_bene
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

data Screen = Welcome | Editing | Save | Open

data Luggage = Luggage { screen   :: Screen
                       , buffer   :: Gtk.TextBuffer
                       , filename :: Maybe FilePath }

type AppState a = State Luggage a

data Event = Closed                            -- ^ The window was closed.
           | OpenFileSelected (Maybe FilePath) -- ^ An open target was selected in the FileChooser.
           | NewClicked                        -- ^ The new document button was clicked.
           | OpenClicked                       -- ^ The open button was clicked.
           | SaveClicked                       -- ^ The save button was clicked.
           | SaveFileSelected (Maybe FilePath) -- ^ A save target was selected in the FileChooser.

-- | Create an empty TextBuffer.
-- [TODO] Remove the use of `unsafePerformIO` here.
createBuffer :: Gtk.TextBuffer
createBuffer = unsafePerformIO $ do
  buffer <- Gtk.textBufferNew . Just =<< markdownTextTagTable
  Gtk.on buffer #endUserAction (renderMarkdown buffer)
  return buffer

-- | Constructs a FileFilter which pertains to Markdown documents.
markdownFileFilter :: IO Gtk.FileFilter
markdownFileFilter = do
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterSetName filt $ Just "Markdown documents"
  Gtk.fileFilterAddMimeType filt "text/markdown"
  Gtk.fileFilterAddMimeType filt "text/x-markdown"
  Gtk.fileFilterAddPattern filt "**/*.{markdown,md}"
  return filt

-- | Create a TextView widget from a given TextBuffer.
editor :: Gtk.TextBuffer -> Widget Event
editor buffer = widget
  Gtk.TextView
  [ afterCreated setBuffer
  , #wrapMode := Gtk.WrapModeWord
  , #margin := 10
  , classes ["editor"]
  ]
  where setBuffer tv = Gtk.textViewSetBuffer tv $ Just buffer

-- | Render Markdown based on the content of a TextBuffer.
renderMarkdown :: Gtk.TextBuffer -> IO ()
renderMarkdown buffer = clearTags buffer >> formatBuffer buffer

-- | Remove all TextTags from a buffer.
clearTags :: Gtk.TextBuffer -> IO ()
clearTags buffer = do
  s <- Gtk.textBufferGetStartIter buffer
  e <- Gtk.textBufferGetEndIter buffer
  Gtk.textBufferRemoveAllTags buffer s e

-- | Retrieve the contents of a TextBuffer.
getBufferContents :: Gtk.TextBuffer -> IO Text
getBufferContents buffer = do
  s <- Gtk.textBufferGetStartIter buffer
  e <- Gtk.textBufferGetEndIter buffer
  Gtk.textBufferGetText buffer s e True


-- | Place a widget inside a BoxChild and allow it to expand.
expandableChild :: Widget a -> BoxChild a
expandableChild =
  BoxChild defaultBoxChildProperties { expand = True, fill = True }

toolbar :: BoxChild Event
toolbar = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, classes ["toolbar"]]
  [widget Gtk.Button [on #clicked SaveClicked, #label := "Save"]]

view' :: AppState (AppView Gtk.Window Event)
view' = do
  s <- gets screen
  b <- gets buffer
  return
    $ bin
        Gtk.Window
        [ #title := "Bene"
        , on #deleteEvent (const (True, Closed))
        , #widthRequest := 480
        , #heightRequest := 300
        ]
    $ case s of
        Welcome ->
          container Gtk.Box [#orientation := Gtk.OrientationHorizontal]
            $ [ expandableChild $ widget
                Gtk.ToolButton
                [ #iconName := "document-new"
                , on #clicked NewClicked
                , classes ["intro"]
                ]
              , expandableChild $ widget
                Gtk.ToolButton
                [ #iconName := "document-open"
                , on #clicked OpenClicked
                , classes ["intro"]
                ]
              ]
        Editing ->
          container Gtk.Box [#orientation := Gtk.OrientationVertical]
            $ [expandableChild $ bin Gtk.ScrolledWindow [] $ editor b, toolbar]
        Save -> widget
          Gtk.FileChooserWidget
          [ #action := Gtk.FileChooserActionSave
          , onM #fileActivated
                (fmap SaveFileSelected . Gtk.fileChooserGetFilename)
          ]
        Open -> widget
          Gtk.FileChooserWidget
          [ #action := Gtk.FileChooserActionOpen
          , onM #fileActivated
                (fmap OpenFileSelected . Gtk.fileChooserGetFilename)
          ]


update' :: Luggage -> Event -> Transition Luggage Event
update' s NewClicked  = Transition s { screen = Editing } $ return Nothing
update' s SaveClicked = Transition s { screen = Save } $ return Nothing
update' s (SaveFileSelected (Just file)) =
  Transition s { screen = Editing } $ do
    T.writeFile file =<< (getBufferContents $ buffer s)
    return Nothing
update' s (SaveFileSelected Nothing) =
  Transition s { screen = Editing } $ return Nothing
update' s OpenClicked = Transition s { screen = Open } $ return Nothing
update' s (OpenFileSelected (Just file)) =
  Transition s { screen = Editing } $ do
    contents <- T.readFile file
    Gtk.textBufferBeginUserAction $ buffer s
    Gtk.textBufferSetText (buffer s) contents $ fromIntegral $ bytes contents
    Gtk.textBufferEndUserAction $ buffer s
    return $ Nothing
update' s (OpenFileSelected Nothing) =
  Transition s { screen = Editing } $ return Nothing
update' _ _ = Exit

main :: IO ()
main = do
  void $ Gtk.init Nothing

  path     <- T.pack <$> getDataFileName "themes/giorno/giorno.css"
  screen   <- maybe (fail "No screen?") return =<< Gdk.screenGetDefault
  provider <- Gtk.cssProviderNew
  settings <- Gtk.settingsGetDefault

  case settings of
    Just settings' -> do
      Gtk.setSettingsGtkCursorBlink settings' False
    Nothing -> return ()

  Gtk.cssProviderLoadFromPath provider path
  Gtk.styleContextAddProviderForScreen
    screen
    provider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    runLoop app
    Gtk.mainQuit
  Gtk.main
 where
  app = App { view         = evalState view'
            , update       = update'
            , inputs       = []
            , initialState = Luggage Welcome createBuffer Nothing
            }
