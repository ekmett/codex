module Main where

import Control.Monad
import System.Environment
import Graphics.Rendering.FreeType.Internal as FT
import Graphics.Rendering.FreeType.Internal.Matrix as M
import Graphics.Rendering.FreeType.Internal.Vector as V
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.Bitmap as B

import Foreign
import Foreign.Marshal
import Foreign.C.String

import System.Exit

import Data.Array.IO as A

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

main :: IO ()
main = do
  let display_width  = 640
      display_height = 480
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
        , y = (display_height - 200) * 64
        })
  (filename:text:_) <- getArgs

  putStrLn $ concat ["Loading file: ", filename]
  putStrLn $ concat ["Drawing text: ", text]

  library <- alloca $ \libraryptr -> do
    putStr "Library ptr: "
    print libraryptr
    runFreeType $ ft_Init_FreeType libraryptr
    peek libraryptr

  face <- alloca $ \faceptr -> do
    putStr "Face ptr: "
    print faceptr
    withCString filename $ \str -> do
      runFreeType $ ft_New_Face library str 0 faceptr
      peek faceptr

  image <- A.newArray
    ((0,0), (fromIntegral display_height - 1, fromIntegral display_width - 1)) 0
    :: IO (IOUArray (Int, Int) Int)

  runFreeType $ ft_Set_Char_Size face (50*64) 0 100 0
  forM_ text $ \c -> do
    withForeignPtr matrix $ \mp ->
      withForeignPtr pen $ \pp -> do
        ft_Set_Transform face mp pp
        slot <- peek $ glyph face
        runFreeType $
          ft_Load_Char face (fromIntegral . fromEnum $ c) ft_LOAD_RENDER
        numFaces <- peek $ num_faces face
        putStrLn $ "face->num_faces = " ++ show numFaces
        v <- peek $ advance slot
        putStrLn $ "advance: " ++ show v
        numGlyphs <- peek $ num_glyphs face
        putStrLn $ "numGlyphs = " ++ show numGlyphs
        pen' <- peek pp
        poke pp $ FT_Vector { x = x v + x pen'
                            , y = y v + y pen' }
        b <- peek $ bitmap slot
        left <- peek $ bitmap_left slot
        top  <- peek $ bitmap_top slot
        let b_top = fromIntegral display_height - top
            b_right = left + width b
            b_bottom = fromIntegral . fromEnum $ b_top + rows b
        unless (b_right >= display_width || b_bottom >= display_height) $
          drawBitmap b image left b_top
  showImage image
  runFreeType $ ft_Done_Face face
  runFreeType $ ft_Done_FreeType library

drawBitmap :: FT_Bitmap -> IOUArray (Int, Int) Int
           -> FT_Int -> FT_Int -> IO ()
drawBitmap bitmap image x y = do
  let xMax = x + width bitmap
      yMax = y + rows bitmap
  forM_ (zip [ x .. xMax - 1] [0 .. ]) $ \(i,p) ->
    forM_ (zip [ y .. yMax - 1] [0 .. ]) $ \(j,q) -> do
      let index = q * width bitmap + p
      v <- readArray image (fromIntegral j, fromIntegral i) :: IO Int
      b <- peek $ (buffer bitmap) `plusPtr` fromIntegral index
      writeArray image (fromIntegral j, fromIntegral i) $ v .|. b

showImage :: IOUArray (Int, Int) Int -> IO ()
showImage image = do
  ((hmin,wmin), (hmax,wmax)) <- getBounds image
  forM_ [ hmin .. hmax - 1 ] $ \i -> do
    forM_ [ wmin .. wmax - 1 ] $ \j -> do
      v <- readArray image (i,j)
      putc v
    putChar '\n'
  where
  putc :: Int -> IO ()
  putc c
    | c == 0    = putChar '0'
    | c < 128   = putChar '+'
    | otherwise = putChar '*'

