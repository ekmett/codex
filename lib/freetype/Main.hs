{-# language DuplicateRecordFields #-}
module Main where

import Control.Monad
import Data.Array.IO as A
import Data.Char
import Data.Int
import Data.Ix
import Data.List
import Data.Traversable
import Foreign
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types
import Graphics.FreeType.Internal as FT
import Graphics.FreeType.Types as FT
import System.Environment
import System.Exit

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
  (filename:degrees:text:_) <- getArgs

  let display_width  = 320
      display_height = 240
      angle  = (Prelude.read degrees / 360) * pi * 2

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

  runFreeType $ setCharSize face (50*64) 0 72 0
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
        drawBitmap b image left $ fromIntegral b_top
  showImage image >>= pretty
  runFreeType $ doneFace face
  runFreeType $ doneFreeType library

pretty :: [String] -> IO ()
pretty = putStr . unlines . fmap (dropWhileEnd isSpace) . dropWhile (all isSpace) . dropWhileEnd (all isSpace)

drawBitmap :: Bitmap -> IOUArray (Int, Int) Int32 -> Int32 -> Int32 -> IO ()
drawBitmap bitmap image x y = do
  let xMax = x + fromIntegral (width bitmap)
      yMax = y + fromIntegral (rows bitmap)
  bounds <- getBounds image
  forM_ (zip [ x .. xMax - 1] [0 .. ]) $ \(i,p) ->
    forM_ (zip [ y .. yMax - 1] [0 .. ]) $ \(j,q) -> do
      let index = q * pitch bitmap + p
      let pt = (fromIntegral j, fromIntegral i)
      when (bounds `inRange` pt) $ do
        v <- readArray image pt
        b <- peek (buffer bitmap `plusPtr` fromIntegral index :: Ptr Word8)
        writeArray image pt $ v .|. fromIntegral b

showImage :: IOUArray (Int, Int) Int32 -> IO [String]
showImage image = do
  ((hmin,wmin), (hmax,wmax)) <- getBounds image
  for [ hmin .. hmax - 1 ] $ \i ->
    for [ wmin .. wmax - 1 ] $ \j ->
      ch <$> readArray image (i,j)
 where
  ch :: Int32 -> Char
  ch c
    | c == 0    = ' '
    | c < 128   = '+'
    | otherwise = '*'

