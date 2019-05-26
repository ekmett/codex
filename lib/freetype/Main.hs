{-# language DuplicateRecordFields #-}
module Main where

import Control.Monad
import System.Environment
import Graphics.FreeType.Internal as FT
import Graphics.FreeType.Types as FT

import Foreign
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types

import System.Exit

import Data.Array.IO as A
import Data.Int

runFreeType :: IO Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

mallocWith :: Storable a => a -> IO (ForeignPtr a)
mallocWith a = do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \p -> poke p a
  pure fp

main :: IO ()
main = do
  let display_width  = 320
      display_height = 240
      angle  = (0 / 360) * pi * 2

  matrix <- mallocWith $ Matrix
    { xx = round $   cos angle * 0x10000
    , xy = round $ -(sin angle * 0x10000)
    , yx = round $   sin angle * 0x10000
    , yy = round $   cos angle * 0x10000
    }
  pen <- mallocWith $ Vector
    { x = 20
    , y = (display_height - 200) * 64
    }

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
    :: IO (IOUArray (Int, Int) Int32)

  runFreeType $ setCharSize face (50*32) 0 72 0
  forM_ text $ \c -> do
    withForeignPtr matrix $ \mp ->
      withForeignPtr pen $ \pp -> do
        setTransform face mp pp
        slot <- peek $ glyph face
        runFreeType $ loadChar face (fromIntegral . fromEnum $ c) LOAD_RENDER
        v <- peek $ advance slot
        pen' <- peek pp
        poke pp $ v + pen'
        b <- peek $ bitmap slot
        left <- fromIntegral <$> peek (bitmap_left slot)
        top  <- fromIntegral <$> peek (bitmap_top slot)
        let b_top = fromIntegral display_height - top
            b_right = left + fromIntegral (width b)
            b_bottom = fromIntegral $ b_top + fromIntegral (rows b)
        unless (b_right >= display_width || b_bottom >= display_height) $
          drawBitmap b image left $ fromIntegral b_top
  showImage image
  runFreeType $ doneFace face
  runFreeType $ doneFreeType library

drawBitmap :: Bitmap -> IOUArray (Int, Int) Int32 -> Int32 -> Int32 -> IO ()
drawBitmap bitmap image x y = do
  let xMax = x + fromIntegral (width bitmap)
      yMax = y + fromIntegral (rows bitmap)
  forM_ (zip [ x .. xMax - 1] [0 .. ]) $ \(i,p) ->
    forM_ (zip [ y .. yMax - 1] [0 .. ]) $ \(j,q) -> do
      let index = q * pitch bitmap + p
      v <- readArray image (fromIntegral j, fromIntegral i) :: IO Int32
      b <- peek (buffer bitmap `plusPtr` fromIntegral index :: Ptr Word8)
      writeArray image (fromIntegral j, fromIntegral i) $ v .|. fromIntegral b

showImage :: IOUArray (Int, Int) Int32 -> IO ()
showImage image = do
  ((hmin,wmin), (hmax,wmax)) <- getBounds image
  forM_ [ hmin .. hmax - 1 ] $ \i -> do
    forM_ [ wmin .. wmax - 1 ] $ \j -> do
      v <- readArray image (i,j)
      putc v
    putChar '\n'
  where
  putc :: Int32 -> IO ()
  putc c
    | c == 0    = putChar ' '
    | c < 128   = putChar '+'
    | otherwise = putChar '*'

