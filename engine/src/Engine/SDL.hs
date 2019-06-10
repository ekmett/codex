{-# language TemplateHaskell #-}
module Engine.SDL
( relativeMouseMode
, SDLException(..)
, AsSDLException(..)
) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens.Type
import Control.Monad
import Data.StateVar
import Foreign.C
import SDL.Raw

newtype SDLException = SDLException String deriving Show

instance Exception SDLException where
  displayException (SDLException s) = "SDL Exception: " ++ s

class AsSDLException t where _SDLException :: Prism' t SDLException
instance AsSDLException SDLException where _SDLException = id
instance AsSDLException SomeException where _SDLException = exception


-- | Treat negative return codes as prompting an error check.
sdl_err :: CInt -> IO ()
sdl_err e
  | e < 0 = do
    msg <- getError >>= peekCString
    clearError
    when (msg /= "") $ throw $ SDLException msg
  | otherwise = return ()

-- | Get/Set relative mouse mode. When enabled we get relative mouse position events even at the screen edge.
relativeMouseMode :: StateVar Bool
relativeMouseMode = StateVar getRelativeMouseMode (setRelativeMouseMode >=> sdl_err)
