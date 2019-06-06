{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StrictData #-}
{-# language FunctionalDependencies #-}
{-# language TemplateHaskell #-}
{-# language MultiWayIf #-}
{-# language FlexibleInstances #-}

-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Details of the implementation.
--
-- The contents of this module do not fall under the PVP. Use at your own risk.

#ifndef HLINT
#include "stb_rect_pack.h"
#endif

module Data.Atlas.Internal
( Atlas(..)
, AtlasContext
, Coord
, Rect
, Node
, heuristicId, Heuristic(..)
, sizeOfAtlas
, sizeOfNode
, sizeOfRect
, Pt(..)
, peekWH, peekXY
, pokeWH, peekMaybeXY
, atlasCtx
) where

import Data.Coerce
import Data.Default
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Word
import Data.Int
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Arr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

type Coord = Word16
data Node
data Rect
data AtlasContext

newtype Atlas s = Atlas (ForeignPtr AtlasContext) deriving (Eq,Ord,Show)

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = TH.lookupValueName (C.unHaskellIdentifier s) >>= \ case
  Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++ ", because it's not in scope. (" ++ err ++ ")"
  Just hsName -> TH.varE hsName

anti :: C.Type C.CIdentifier -> TH.TypeQ -> TH.ExpQ -> C.SomeAntiQuoter
anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
  { C.aqParser = C.parseIdentifier <&> \hId -> (C.mangleHaskellIdentifier hId, cTy, hId)
  , C.aqMarshaller = \_ _ _ cId -> (,) <$> hsTyQ <*> [|$w (coerce $(getHsVariable "freeTypeCtx" cId))|]
  }

atlasCtx :: C.Context
atlasCtx = mempty 
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "stbrp_context", [t|AtlasContext|])
    , (C.TypeName "stbrp_rect",    [t|Rect|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("atlas", anti (C.Ptr [] $ C.TypeSpecifier mempty $ C.TypeName "stbrp_context") [t|Ptr AtlasContext|] [|withForeignPtr|])
    ]
  }

data Heuristic
  = BottomLeft -- bottom-left sort-height
  | BestFirst -- best first sort-height
  deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded)

instance Default Heuristic where
  def = BottomLeft

-- | Use and cast back and forth to ints instead for more natural API?
data Pt = Pt Int Int deriving (Eq,Ord,Show,Read)

instance Num Pt where
  Pt a b + Pt c d = Pt (a + c) (b + d)
  Pt a b - Pt c d = Pt (a - c) (b - d)
  Pt a b * Pt c d = Pt (a * c) (b * d)
  abs (Pt a b) = Pt (abs a) (abs b)
  signum (Pt a b) = Pt (signum a) (signum b)
  negate (Pt a b) = Pt (negate a) (negate b)
  fromInteger n = Pt (fromInteger n) (fromInteger n)

#ifndef HLINT

heuristicId :: Heuristic -> CInt
heuristicId BottomLeft = #const STBRP_HEURISTIC_Skyline_BL_sortHeight
heuristicId BestFirst  = #const STBRP_HEURISTIC_Skyline_BF_sortHeight

sizeOfAtlas :: Int
sizeOfAtlas = #size stbrp_context

sizeOfNode :: Int
sizeOfNode = #size stbrp_node

sizeOfRect :: Int
sizeOfRect = #size stbrp_rect

peekWH :: Ptr Rect -> IO Pt
peekWH p = (\(w :: Coord) (h :: Coord) -> Pt (fromIntegral w) (fromIntegral h))
  <$> (#peek stbrp_rect, w) p
  <*> (#peek stbrp_rect, h) p

pokeWH :: Ptr Rect -> Pt -> IO ()
pokeWH p (Pt w h) = do
  (#poke stbrp_rect, id) p (0 :: Int32)
  (#poke stbrp_rect, x) p (0 :: Coord)
  (#poke stbrp_rect, y) p (0 :: Coord)
  (#poke stbrp_rect, w) p (fromIntegral w :: Coord)
  (#poke stbrp_rect, h) p (fromIntegral h :: Coord)
  (#poke stbrp_rect, was_packed) p (0 :: Int32)

peekXY :: Ptr Rect -> IO Pt
peekXY p = (\(w :: Coord) (h :: Coord) -> Pt (fromIntegral w) (fromIntegral h))
  <$> (#peek stbrp_rect, x) p
  <*> (#peek stbrp_rect, y) p

peekMaybeXY :: Ptr Rect -> IO (Maybe Pt)
peekMaybeXY p = (#peek stbrp_rect, was_packed) p >>= \case
  (0 :: Int32) -> pure Nothing
  _            -> Just <$> peekXY p

#endif
