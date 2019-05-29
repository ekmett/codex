{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
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

import Control.Monad.IO.Class
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

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

key_create :: MonadIO m => m (Key a)
key_create = liftIO $ Key <$> mallocForeignPtrBytes 1

key_create_many :: (MonadIO m, FTraversable t) => t proxy -> m (t Key)
key_create_many types = liftIO $
  evalState (ftraverse step types) <$> mallocForeignPtrBytes (flength types) where
    step :: proxy a -> State (ForeignPtr OpaqueKey) (Key a)
    step _ = state $ \s -> (Key s, plusForeignPtr s 1)

class IsObject t where
  _reference :: t -> IO (Ptr t)
  _destroy :: t -> IO ()
  _set_user_data :: t -> Key () -> Ptr () -> FinalizerPtr () -> CInt -> IO CInt
  _get_user_data :: t -> Key () -> IO (Ptr ())

object_reference :: (MonadIO m, IsObject t) => t -> m (Ptr t)
object_reference = liftIO . _reference
{-# inline object_reference #-}

object_destroy :: (MonadIO m, IsObject t) => t -> m ()
object_destroy = liftIO . _destroy
{-# inline object_destroy #-}

object_set_user_data :: (MonadIO m, IsObject t) => t -> Key a -> a -> Bool -> m Bool
object_set_user_data t k v replace = liftIO $ do
  v' <- newStablePtr v
  cbool <$> _set_user_data t (coerce k) (castStablePtrToPtr v') hs_free_stable_ptr (boolc replace)
{-# inline object_set_user_data #-}

object_get_user_data :: (MonadIO m, IsObject t) => t -> Key a -> m (Maybe a)
object_get_user_data t k = liftIO $ _get_user_data t (coerce k) >>= maybePeek (deRefStablePtr . castPtrToStablePtr)
{-# inline object_get_user_data #-}

instance IsObject Blob where
  _reference b = [C.exp|hb_blob_t * { hb_blob_reference($blob:b) }|]
  _destroy b = [C.block|void { hb_blob_destroy($blob:b); }|]
  _get_user_data b k = [C.exp|void * { hb_blob_get_user_data($blob:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_blob_set_user_data($blob:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Buffer where
  _reference b = [C.exp|hb_buffer_t * { hb_buffer_reference($buffer:b) }|]
  _destroy b = [C.block|void { hb_buffer_destroy($buffer:b); }|]
  _get_user_data b k = [C.exp|void * { hb_buffer_get_user_data($buffer:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_buffer_set_user_data($buffer:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Face where
  _reference b = liftIO [C.exp|hb_face_t * { hb_face_reference($face:b) }|]
  _destroy b = liftIO [C.block|void { hb_face_destroy($face:b); }|]
  _get_user_data b k = [C.exp|void * { hb_face_get_user_data($face:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_face_set_user_data($face:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Font where
  _reference b = [C.exp|hb_font_t * { hb_font_reference($font:b) }|]
  _destroy b = [C.block|void { hb_font_destroy($font:b); }|]
  _get_user_data b k = [C.exp|void * { hb_font_get_user_data($font:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_font_set_user_data($font:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject FontFuncs where
  _reference b = [C.exp|hb_font_funcs_t * { hb_font_funcs_reference($font-funcs:b) }|]
  _destroy b = [C.block|void { hb_font_funcs_destroy($font-funcs:b); }|]
  _get_user_data b k = [C.exp|void * { hb_font_funcs_get_user_data($font-funcs:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_font_funcs_set_user_data($font-funcs:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Map where
  _reference b = [C.exp|hb_map_t * { hb_map_reference($map:b) }|]
  _destroy b = [C.block|void { hb_map_destroy($map:b); }|]
  _get_user_data b k = [C.exp|void * { hb_map_get_user_data($map:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_map_set_user_data($map:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject Set where
  _reference b = [C.exp|hb_set_t * { hb_set_reference($set:b) }|]
  _destroy b = [C.block|void { hb_set_destroy($set:b); }|]
  _get_user_data b k = [C.exp|void * { hb_set_get_user_data($set:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_set_set_user_data($set:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject ShapePlan where
  _reference uf = [C.exp|hb_shape_plan_t * { hb_shape_plan_reference($shape-plan:uf) }|]
  _destroy uf = [C.block|void { hb_shape_plan_destroy($shape-plan:uf); }|]
  _get_user_data b k = [C.exp|void * { hb_shape_plan_get_user_data($shape-plan:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_shape_plan_set_user_data($shape-plan:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

instance IsObject UnicodeFuncs where
  _reference uf = [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_reference($unicode-funcs:uf) }|]
  _destroy uf = [C.block|void { hb_unicode_funcs_destroy($unicode-funcs:uf); }|]
  _get_user_data b k = [C.exp|void * { hb_unicode_funcs_get_user_data($unicode-funcs:b,$key:k) }|]
  _set_user_data b k v d replace = [C.exp|hb_bool_t { hb_unicode_funcs_set_user_data($unicode-funcs:b,$key:k,$(void*v),$(hb_destroy_func_t d),$(hb_bool_t replace)) }|]

