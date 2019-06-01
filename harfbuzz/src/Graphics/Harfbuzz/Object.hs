{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language QuasiQuotes #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.Object
( IsObject(..)
, Key
, key_create
, key_create_many
, object_reference
, object_destroy
, object_set_user_data
, object_get_user_data
) where

import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.HKD
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.Marshal.Utils
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

key_create :: PrimMonad m => m (Key a)
key_create = unsafeIOToPrim $ Key <$> mallocForeignPtrBytes 1

key_create_many :: (PrimMonad m, FTraversable t) => t proxy -> m (t Key)
key_create_many types = unsafeIOToPrim $
  evalState (ftraverse step types) <$> mallocForeignPtrBytes (flength types) where
    step :: proxy a -> State (ForeignPtr Key_) (Key a)
    step _ = state $ \s -> (Key s, plusForeignPtr s 1)

class IsObject t where
  type Rec t :: *
  _reference :: t s -> IO (Ptr (Rec t))
  _destroy :: t s -> IO ()
  _set_user_data :: t s -> Key () -> Ptr () -> FinalizerPtr () -> CInt -> IO CInt
  _get_user_data :: t s -> Key () -> IO (Ptr ())

object_reference :: (PrimMonad m, IsObject t) => t (PrimState m) -> m (Ptr (Rec t))
object_reference = unsafeIOToPrim . _reference
{-# inline object_reference #-}

object_destroy :: (PrimMonad m, IsObject t) => t (PrimState m) -> m ()
object_destroy = unsafeIOToPrim . _destroy
{-# inline object_destroy #-}

object_set_user_data :: (PrimMonad m, IsObject t) => t (PrimState m) -> Key a -> a -> Bool -> m Bool
object_set_user_data t k v replace = unsafeIOToPrim $ do
  v' <- newStablePtr v
  cbool <$> _set_user_data t (coerce k) (castStablePtrToPtr v') hs_free_stable_ptr (boolc replace)
{-# inline object_set_user_data #-}

object_get_user_data :: (PrimMonad m, IsObject t) => t (PrimState m) -> Key a -> m (Maybe a)
object_get_user_data t k = unsafeIOToPrim $ _get_user_data t (coerce k) >>= maybePeek (deRefStablePtr . castPtrToStablePtr)
{-# inline object_get_user_data #-}

instance IsObject Blob where
  type Rec Blob = Blob_
  _reference b = [C.exp|hb_blob_t * { hb_blob_reference($blob:b) }|]
  _destroy b = [C.block|void { hb_blob_destroy($blob:b); }|]
  _get_user_data b k = [C.exp|void * { hb_blob_get_user_data($blob:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_blob_set_user_data($blob:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Buffer where
  type Rec Buffer = Buffer_
  _reference b = [C.exp|hb_buffer_t * { hb_buffer_reference($buffer:b) }|]
  _destroy b = [C.block|void { hb_buffer_destroy($buffer:b); }|]
  _get_user_data b k = [C.exp|void * { hb_buffer_get_user_data($buffer:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_buffer_set_user_data($buffer:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Face where
  type Rec Face = Face_
  _reference b = unsafeIOToPrim [C.exp|hb_face_t * { hb_face_reference($face:b) }|]
  _destroy b = unsafeIOToPrim [C.block|void { hb_face_destroy($face:b); }|]
  _get_user_data b k = [C.exp|void * { hb_face_get_user_data($face:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_face_set_user_data($face:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Font where
  type Rec Font = Font_
  _reference b = [C.exp|hb_font_t * { hb_font_reference($font:b) }|]
  _destroy b = [C.block|void { hb_font_destroy($font:b); }|]
  _get_user_data b k = [C.exp|void * { hb_font_get_user_data($font:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_font_set_user_data($font:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject FontFuncs where
  type Rec FontFuncs = FontFuncs_
  _reference b = [C.exp|hb_font_funcs_t * { hb_font_funcs_reference($font-funcs:b) }|]
  _destroy b = [C.block|void { hb_font_funcs_destroy($font-funcs:b); }|]
  _get_user_data b k = [C.exp|void * { hb_font_funcs_get_user_data($font-funcs:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_font_funcs_set_user_data($font-funcs:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Map where
  type Rec Map = Map_
  _reference b = [C.exp|hb_map_t * { hb_map_reference($map:b) }|]
  _destroy b = [C.block|void { hb_map_destroy($map:b); }|]
  _get_user_data b k = [C.exp|void * { hb_map_get_user_data($map:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_map_set_user_data($map:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Set where
  type Rec Set = Set_
  _reference b = [C.exp|hb_set_t * { hb_set_reference($set:b) }|]
  _destroy b = [C.block|void { hb_set_destroy($set:b); }|]
  _get_user_data b k = [C.exp|void * { hb_set_get_user_data($set:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_set_set_user_data($set:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject ShapePlan where
  type Rec ShapePlan = ShapePlan_
  _reference uf = [C.exp|hb_shape_plan_t * { hb_shape_plan_reference($shape-plan:uf) }|]
  _destroy uf = [C.block|void { hb_shape_plan_destroy($shape-plan:uf); }|]
  _get_user_data b k = [C.exp|void * { hb_shape_plan_get_user_data($shape-plan:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_shape_plan_set_user_data($shape-plan:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject UnicodeFuncs where
  type Rec UnicodeFuncs = UnicodeFuncs_
  _reference uf = [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_reference($unicode-funcs:uf) }|]
  _destroy uf = [C.block|void { hb_unicode_funcs_destroy($unicode-funcs:uf); }|]
  _get_user_data b k = [C.exp|void * { hb_unicode_funcs_get_user_data($unicode-funcs:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_unicode_funcs_set_user_data($unicode-funcs:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

