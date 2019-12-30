{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.STB.TrueType.Internal where
  
import qualified Data.Map as Map
import Linear.V4 (V4)
import Linear.V2 (V2)
import Foreign.C.Types (CShort,CFloat)

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C

C.include "<stb_rect_pack.h>"
C.include "<stb_truetype.h>"

data PackedChar = PackedChar
  { _packedChar_boundingBox :: V4 CShort
  , _packedChar_off1 :: V2 CFloat
  , _packedChar_xadvance :: CFloat
  , _packedChar_off2 :: V2 CFloat
  }

stbTrueTypeCtx :: C.Context
stbTrueTypeCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "stbtt_packedchar", [t|PackedChar|])
    ]
  }
