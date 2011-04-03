module Main where

import Control.Monad
import System.Environment
import Graphics.Rendering.FreeType.Internal

import Foreign
import Foreign.Marshal

main :: IO ()
main = do
  let width  = 640
      height = 480
      angle  = (25 / 360) * pi * 2
  matrix <- mallocForeignPtr
  pen    <- mallocForeignPtr
  withForeignPtr matrix $ \p-> poke p (FT_Matrix
        { xx = round $   cos angle * 0x10000
        , xy = round $ -(sin angle * 0x10000)
        , yx = round $   sin angle * 0x10000
        , yy = round $   cos angle * 0x10000
        })
  withForeignPtr pen $ \p -> poke p (FT_Vector
        { x = 300 * 64
        , y = (height - 200) * 64
        })
  (filename:text:_) <- getArgs
  Right library <- ft_Init_FreeType
  Right face    <- ft_New_Face library filename 0
  ft_Set_Char_Size face (50*64) 0 100 0
  forM_ text $ \c -> do
    withForeignPtr matrix $ \mp ->
      withForeignPtr pen $ \pp -> do
        ft_Set_Transform face mp pp
        ft_Load_Char face (fromIntegral . fromEnum $ c) ft_LOAD_RENDER
    -- drawBitmap



  
  
