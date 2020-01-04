{- |
 Module : Notewell.Helpers
 Description : Helper function module.
 Copyright : Rose <rose@empty.town>
 License : BSD3
 Maintainer : rose@empty.town
-}

module Notewell.Helpers where

import Control.Monad (void)

-- | Executes a monadic action on the value of a Maybe, if one exists.
whenM :: Monad m => Maybe a -> (a -> m b) -> m ()
whenM value block = maybe (return ()) (void . block) value
