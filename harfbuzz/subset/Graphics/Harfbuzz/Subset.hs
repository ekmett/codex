{-# language TypeFamilies #-}
{-# language ForeignFunctionInterface #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language DeriveAnyClass #-}
module Graphics.Harfbuzz.Subset
( Input
, input_create, CreateFailed(..)
, input_unicode_set
, input_glyph_set
, input_nameid_set
, input_drop_tables_set
, input_drop_hints -- statevar
, input_desubroutinize -- statevar
, input_retain_gids -- statevar
, subset
) where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Primitive
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Primitive.StateVar
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Graphics.Harfbuzz.Internal
--import Graphics.Harfbuzz.Object
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data Input_

C.context $ C.baseCtx <> C.fptrCtx <> harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [(C.TypeName "hb_subset_input_t", [t|Input_|])
    ]
  }

C.include "<hb.h>"
C.include "<hb-subset.h>"

newtype Input s = Input (ForeignPtr Input_)

{-
instance IsObject Input where
  type Rec Input = Input_
  _reference (Input b) = [C.exp|hb_subset_input_t * { hb_subset_input_reference($fptr-ptr:(hb_subset_input_t * b)) }|]
  _destroy (Input b) = [C.block|void { hb_subset_input_destroy($fptr-ptr:(hb_subset_input_t * b)); }|]
  _get_user_data (Input b) k = [C.exp|void * { hb_subset_input_get_user_data($fptr-ptr:(hb_subset_input_t * b),$key:k) }|]
  _set_user_data (Input b) k v d replace =
   [C.exp|hb_bool_t {
     hb_subset_input_set_user_data(
       $fptr-ptr:(hb_subset_input_t * b),
       $key:k,
       $(void*v),
       $(hb_destroy_func_t d),
       $(hb_bool_t replace)
     )
   }|]
-}

data CreateFailed = CreateFailed deriving (Show,Exception)

-- TH.forImpD TH.CCall TH.Safe "hb-subset.h &" (mkName "hb_subset_input_destroy) [t| FunPtr (Ptr Input_ -> IO ()) |] >>= TH.addTopDecls . pure
foreign import ccall "hb-subset.h &" hb_subset_input_destroy :: FunPtr (Ptr Input_ -> IO ())

input_create :: PrimMonad m => m (Input (PrimState m))
input_create = unsafeIOToPrim $ do
  p <- [C.exp|hb_subset_input_t * { hb_subset_input_create_or_fail() }|]
  when (p == nullPtr) $ throwIO CreateFailed
  Input <$> newForeignPtr hb_subset_input_destroy p

childSet :: Input s -> Ptr Set_ -> Set s
childSet (Input fp) p = Set $ fp `plusForeignPtr` minusPtr p (unsafeForeignPtrToPtr fp)

input_unicode_set :: PrimMonad m => Input (PrimState m) -> m (Set (PrimState m))
input_unicode_set i@(Input p) = unsafeIOToPrim $
  [C.exp|hb_set_t * { hb_subset_input_unicode_set($fptr-ptr:(hb_subset_input_t * p)) }|] <&> childSet i

input_glyph_set :: PrimMonad m => Input (PrimState m) -> m (Set (PrimState m))
input_glyph_set i@(Input p) = unsafeIOToPrim $
  [C.exp|hb_set_t * { hb_subset_input_glyph_set($fptr-ptr:(hb_subset_input_t * p)) }|] <&> childSet i

-- requires harfbuzz 2.5.0+
input_nameid_set :: PrimMonad m => Input (PrimState m) -> m (Set (PrimState m))
input_nameid_set i@(Input p) = unsafeIOToPrim $
  [C.exp|hb_set_t * { hb_subset_input_nameid_set($fptr-ptr:(hb_subset_input_t * p)) }|] <&> childSet i

input_drop_tables_set :: PrimMonad m => Input (PrimState m) -> m (Set (PrimState m))
input_drop_tables_set i@(Input p) = unsafeIOToPrim $
  [C.exp|hb_set_t * { hb_subset_input_drop_tables_set($fptr-ptr:(hb_subset_input_t * p)) }|] <&> childSet i

unsafeStateVar :: IO a -> (a -> IO ()) -> StateVar s a
unsafeStateVar g s = StateVar (unsafeIOToPrim g) (unsafeIOToPrim . s)

input_drop_hints :: Input s -> StateVar s Bool
input_drop_hints (Input i) = unsafeStateVar g s where
  g = [C.exp|hb_bool_t { hb_subset_input_get_drop_hints($fptr-ptr:(hb_subset_input_t * i)) }|] <&> (0/=)
  s (fromIntegral . fromEnum -> v) = [C.block|void { hb_subset_input_set_drop_hints($fptr-ptr:(hb_subset_input_t * i),$(hb_bool_t v)); }|]

input_desubroutinize :: Input s -> StateVar s Bool
input_desubroutinize (Input i) = unsafeStateVar g s where
  g = [C.exp|hb_bool_t { hb_subset_input_get_desubroutinize($fptr-ptr:(hb_subset_input_t * i)) }|] <&> (0/=)
  s (fromIntegral . fromEnum -> v) = [C.block|void { hb_subset_input_set_desubroutinize($fptr-ptr:(hb_subset_input_t * i),$(hb_bool_t v)); }|]

-- retain the glyph ids?
input_retain_gids :: Input s -> StateVar s Bool
input_retain_gids (Input i) = unsafeStateVar g s where
  g = [C.exp|hb_bool_t { hb_subset_input_get_retain_gids($fptr-ptr:(hb_subset_input_t * i)) }|] <&> (0/=)
  s (fromIntegral . fromEnum -> v) = [C.block|void { hb_subset_input_set_retain_gids($fptr-ptr:(hb_subset_input_t * i),$(hb_bool_t v)); }|]

subset :: PrimMonad m => Face (PrimState m) -> Input (PrimState m) -> m (Face (PrimState m))
subset face (Input i) = unsafeIOToPrim $ do
  [C.exp|hb_face_t * { hb_subset($face:face,$fptr-ptr:(hb_subset_input_t * i)) }|] >>= foreignFace
