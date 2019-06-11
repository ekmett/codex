{-# language TemplateHaskell #-}
{-# language BlockArguments #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.Unicode
( UnicodeCombiningClass(..)
, UnicodeFuncs
, UnicodeGeneralCategory(..)
, unicode_funcs_create
, unicode_funcs_get_default
, unicode_funcs_get_parent
, unicode_funcs_is_immutable
, unicode_funcs_make_immutable
, unicode_funcs_set_combining_class_func
, unicode_funcs_set_compose_func
, unicode_funcs_set_decompose_func
, unicode_funcs_set_general_category_func
, unicode_funcs_set_mirroring_func
, unicode_funcs_set_script_func

, unicode_combining_class
, unicode_compose
, unicode_decompose
, unicode_general_category
, unicode_mirroring
, unicode_script
) where

import Control.Monad.Primitive
import Data.Functor ((<&>))
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as U

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

unicode_funcs_create :: PrimMonad m => UnicodeFuncs (PrimState m) -> m (UnicodeFuncs (PrimState m))
unicode_funcs_create parent = unsafeIOToPrim $
  [U.exp|hb_unicode_funcs_t * {
    hb_unicode_funcs_create(hb_unicode_funcs_reference($unicode-funcs:parent))
  }|] >>= foreignUnicodeFuncs

unicode_funcs_get_default :: PrimMonad m => m (UnicodeFuncs (PrimState m))
unicode_funcs_get_default = unsafeIOToPrim $
  [U.exp|hb_unicode_funcs_t * { hb_unicode_funcs_reference(hb_unicode_funcs_get_default()) }|] >>= foreignUnicodeFuncs

unicode_funcs_get_parent :: PrimMonad m => UnicodeFuncs (PrimState m) -> m (UnicodeFuncs (PrimState m))
unicode_funcs_get_parent u = unsafeIOToPrim $
  [U.block|hb_unicode_funcs_t * {
    hb_unicode_funcs_t * p = hb_unicode_funcs_get_parent($unicode-funcs:u);
    return hb_unicode_funcs_reference(p);
  }|] >>= foreignUnicodeFuncs

unicode_funcs_is_immutable :: PrimMonad m => UnicodeFuncs (PrimState m) -> m Bool
unicode_funcs_is_immutable b = unsafeIOToPrim $ [U.exp|hb_bool_t { hb_unicode_funcs_is_immutable($unicode-funcs:b) }|] <&> cbool

unicode_funcs_make_immutable :: PrimMonad m => UnicodeFuncs (PrimState m) -> m ()
unicode_funcs_make_immutable b = unsafeIOToPrim [U.block|void { hb_unicode_funcs_make_immutable($unicode-funcs:b); }|]

unicode_funcs_set_combining_class_func :: PrimBase m => UnicodeFuncs (PrimState m) -> (Char -> m UnicodeCombiningClass) -> m ()
unicode_funcs_set_combining_class_func uf fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkUnicodeCombiningClassFunc \ _ c _ -> unsafePrimToIO (fun c)
  [C.block|void {
    hb_unicode_combining_class_func_t f = $(hb_unicode_combining_class_func_t f);
    hb_unicode_funcs_set_combining_class_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_compose_func :: PrimBase m => UnicodeFuncs (PrimState m) -> (Char -> Char -> m (Maybe Char)) -> m ()
unicode_funcs_set_compose_func uf fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkUnicodeComposeFunc \ _ a b c _ -> unsafePrimToIO (fun a b) >>= \case
     Nothing -> pure $ boolc False
     Just ab -> boolc True <$ poke c ab
  [C.block|void {
    hb_unicode_compose_func_t f = $(hb_unicode_compose_func_t f);
    hb_unicode_funcs_set_compose_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_decompose_func :: PrimBase m => UnicodeFuncs (PrimState m) -> (Char -> m (Maybe (Char,Char))) -> m ()
unicode_funcs_set_decompose_func uf fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkUnicodeDecomposeFunc \ _ a pb pc _ -> unsafePrimToIO (fun a) >>= \case
     Nothing -> pure $ boolc False
     Just (b,c) -> boolc True <$ (poke pb b *> poke pc c)
  [C.block|void {
    hb_unicode_decompose_func_t f = $(hb_unicode_decompose_func_t f);
    hb_unicode_funcs_set_decompose_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_general_category_func :: PrimBase m => UnicodeFuncs (PrimState m) -> (Char -> m UnicodeGeneralCategory) -> m ()
unicode_funcs_set_general_category_func uf fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkUnicodeGeneralCategoryFunc \ _ c _ -> unsafePrimToIO (fun c)
  [C.block|void {
    hb_unicode_general_category_func_t f = $(hb_unicode_general_category_func_t f);
    hb_unicode_funcs_set_general_category_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_mirroring_func :: PrimBase m => UnicodeFuncs (PrimState m) -> (Char -> m Char) -> m ()
unicode_funcs_set_mirroring_func uf fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkUnicodeMirroringFunc \ _ a _ -> unsafePrimToIO (fun a)
  [C.block|void {
    hb_unicode_mirroring_func_t f = $(hb_unicode_mirroring_func_t f);
    hb_unicode_funcs_set_mirroring_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_funcs_set_script_func :: PrimBase m => UnicodeFuncs (PrimState m) -> (Char -> m Script) -> m ()
unicode_funcs_set_script_func uf fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkUnicodeScriptFunc \ _ a _ -> unsafePrimToIO (fun a)
  [C.block|void {
    hb_unicode_script_func_t f = $(hb_unicode_script_func_t f);
    hb_unicode_funcs_set_script_func($unicode-funcs:uf,f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|]

unicode_combining_class :: PrimMonad m => UnicodeFuncs (PrimState m) -> Char -> m UnicodeCombiningClass
unicode_combining_class uf (c2w -> codepoint) = unsafeIOToPrim [C.exp|hb_unicode_combining_class_t { hb_unicode_combining_class($unicode-funcs:uf,$(hb_codepoint_t codepoint)) }|]

unicode_compose :: PrimMonad m => UnicodeFuncs (PrimState m) -> Char -> Char -> m (Maybe Char)
unicode_compose uf (c2w -> a) (c2w -> b) = unsafeIOToPrim $ alloca \c -> do
  ok <- [C.exp|hb_bool_t { hb_unicode_compose($unicode-funcs:uf,$(hb_codepoint_t a),$(hb_codepoint_t b),$(hb_codepoint_t * c)) }|]
  if cbool ok then Just . w2c <$> peek c else pure Nothing

unicode_decompose :: PrimMonad m => UnicodeFuncs (PrimState m) -> Char -> m (Maybe (Char, Char))
unicode_decompose uf (c2w -> a) = unsafeIOToPrim $ allocaArray 2 \ pbc -> do
  ok <- [C.block|hb_bool_t {
    hb_codepoint_t * pbc = $(hb_codepoint_t * pbc);
    return hb_unicode_decompose($unicode-funcs:uf,$(hb_codepoint_t a),pbc,pbc+1);
  }|]
  if cbool ok then do
    b <- peek pbc
    c <- peek (advancePtr pbc 1)
    pure $ Just (w2c b, w2c c)
  else pure Nothing

unicode_general_category :: PrimMonad m => UnicodeFuncs (PrimState m) -> Char -> m UnicodeGeneralCategory
unicode_general_category uf (c2w -> codepoint) = unsafeIOToPrim [C.exp|hb_unicode_general_category_t { hb_unicode_general_category($unicode-funcs:uf,$(hb_codepoint_t codepoint)) }|]

unicode_mirroring :: PrimMonad m => UnicodeFuncs (PrimState m) -> Char -> m Char
unicode_mirroring uf (c2w -> a) = unsafeIOToPrim $ [C.exp|hb_codepoint_t { hb_unicode_mirroring($unicode-funcs:uf,$(hb_codepoint_t a)) }|] <&> w2c

unicode_script :: PrimMonad m => UnicodeFuncs (PrimState m) -> Char -> m Script
unicode_script uf (c2w -> a) = unsafeIOToPrim [C.exp|hb_script_t { hb_unicode_script($unicode-funcs:uf,$(hb_codepoint_t a)) }|]

