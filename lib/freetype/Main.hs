module Main where

import Control.Monad
import System.Environment
import Graphics.FreeType.Internal as FT
import Graphics.FreeType.Matrix as M
import Graphics.FreeType.Vector as V
import Graphics.FreeType.GlyphSlot as GS
import Graphics.FreeType.Types as PT
import Graphics.FreeType.Face as F
import Graphics.FreeType.Bitmap as B

import Foreign
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types

import System.Exit

import Data.Array.IO as A

runFreeType :: IO Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

main :: IO ()
main = do
  let display_width  = 320
      display_height = 240
      angle  = (0 / 360) * pi * 2
  matrix <- mallocForeignPtr
  pen    <- mallocForeignPtr
  withForeignPtr matrix $ \p-> poke p (Matrix
        { xx = round $   cos angle * 0x10000
        , xy = round $ -(sin angle * 0x10000)
        , yx = round $   sin angle * 0x10000
        , yy = round $   cos angle * 0x10000
        })
  withForeignPtr pen $ \p -> poke p (Vector
        { x = 20
        , y = (display_height - 200) * 64
        })
  (filename:text:_) <- getArgs

  putStrLn $ concat ["Loading file: ", filename]
  putStrLn $ concat ["Drawing text: ", text]

  library <- alloca $ \libraryptr -> do
    putStr "Library ptr: "
    print libraryptr
    runFreeType $ initFreeType libraryptr
    peek libraryptr

  face <- alloca $ \faceptr -> do
    putStr "Face ptr: "
    print faceptr
    withCString filename $ \str -> do
      runFreeType $ newFace library str 0 faceptr
      peek faceptr

  image <- A.newArray
    ((0,0), (fromIntegral display_height - 1, fromIntegral display_width - 1)) 0
    :: IO (IOUArray (Int, Int) Int)

  runFreeType $ setCharSize face (50*64) 0 100 0
  forM_ text $ \c -> do
    withForeignPtr matrix $ \mp ->
      withForeignPtr pen $ \pp -> do
        setTransform face mp pp
        slot <- peek $ glyph face
        runFreeType $
          loadChar face (fromIntegral . fromEnum $ c) LOAD_RENDER
        numFaces <- peek $ num_faces face
        putStrLn $ "face->num_faces = " ++ show numFaces
        v <- peek $ advance slot
        putStrLn $ "advance: " ++ show v
        numGlyphs <- peek $ num_glyphs face
        putStrLn $ "numGlyphs = " ++ show numGlyphs
        pen' <- peek pp
        poke pp $ Vector { x = x v + x pen', y = y v + y pen' }
        b <- peek $ bitmap slot
        left <- peek $ bitmap_left slot
        top  <- peek $ bitmap_top slot
        let b_top = fromIntegral display_height - top
            b_right = left + width b
            b_bottom = fromIntegral . fromEnum $ b_top + rows b
        unless (b_right >= display_width || b_bottom >= display_height) $
          drawBitmap b image left b_top
  showImage image
  runFreeType $ doneFace face
  runFreeType $ doneFreeType library

drawBitmap :: Bitmap -> IOUArray (Int, Int) Int -> CInt -> CInt -> IO ()
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

