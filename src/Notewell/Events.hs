{- |
 Module : Notewell.Events
 Description : The program's event type and its constructors.
 Copyright : Rose <rose@empty.town>
 License : BSD3
 Maintainer : rose@empty.town
-}
module Notewell.Events where

import Data.Either      (fromRight)
import Notewell.Metrics
import Notewell.Theming
import System.FilePath.Posix ((</>))

data ThemeType = Light | Dark

data Event = Closed                            -- ^ The window was closed.
           | OpenFileSelected (Maybe FilePath) -- ^ An open target was selected in the FileChooser.
           | NewClicked                        -- ^ The new document button was clicked.
           | OpenClicked                       -- ^ The open button was clicked.
           | SaveClicked                       -- ^ The save button was clicked.
           | SaveFileSelected (Maybe FilePath) -- ^ A save target was selected in the FileChooser.
           | UpdateMetrics Metrics             -- ^ The buffer was updated and analyzed.
           | Render                            -- ^ The Markdown needs re-rendering.
           | ToggleTheme ThemeType

fromBool :: Bool -> ThemeType
fromBool False = Light
fromBool True  = Dark

-- | Set the default light or dark theme according to a ThemeType.
getDefaultTheme :: ThemeType -> IO Theme
getDefaultTheme Light = fromRight defaultTheme <$> readTheme ("themes" </> "giorno.json")
getDefaultTheme Dark  = fromRight defaultTheme <$> readTheme ("themes" </> "notte.json")
