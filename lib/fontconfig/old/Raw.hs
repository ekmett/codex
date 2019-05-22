--------------------------------------------------------------------------------
-- * Freetype Font Faces
--------------------------------------------------------------------------------

data Face

-- c_freetypeCharIndex :: Ptr Face -> Char -> IO CUInt
-- c_freetypeCharSet :: Ptr Face -> Ptr Blanks -> IO (Ptr CharSet)
-- c_freetypeQuery :: CString -> CInt -> Ptr Blanks -> Ptr CInt -> IO (Ptr Pattern)

--------------------------------------------------------------------------------
-- * Types and Values
--------------------------------------------------------------------------------

foreign import ccall "fontconfig/fontconfig.h FcValueDestroy" c_valueDestroy :: Ptr Value -> IO ()
foreign import ccall "fontconfig/fontconfig.h FcValueSave" c_valueSave :: Ptr Value -> IO (Ptr Value)
foreign import ccall "fontconfig/fontconfig.h FcValueEqual" c_valueEqual :: Ptr Value -> Ptr Value -> IO Bool
foreign import ccall "fontconfig/fontconfig.h FcValuePrint" c_valuePrint :: Ptr Value -> IO ()

data Value f =
  = TypeVoid !(f (Ptr ()))
  | TypeInteger !(f Int)
  | TypeDouble !(f Double)
  | TypeString !(f String)
  | TypeBool !(f Bool)
  | TypeMatrix !(f Matrix)
  | TypeCharSet !(f CharSet)
  | TypeFace !(f (Ptr Face))
  | TypeLangSet !(f LangSet)
  | TypeRange !(f Range)

-- instance FFunctor Value where ffmap = mapValue -- move to UI.HKD or move that upstream
mapValue :: (forall a. f a -> g a) -> Value f -> Value g
mapValue f (TypeVoid g)    = TypeVoid (f g)
mapValue f (TypeInteger i) = TypeInteger (f i)
mapValue f (TypeDouble d)  = TypeDouble (f d)
mapValue f (TypeString s)  = TypeString (f s)
mapValue f (TypeBool b)    = TypeBool (f b)
mapValue f (TypeMatrix m)  = TypeMatrix (f m)
mapValue f (TypeCharSet s) = TypeCharSet (f s)
mapValue f (TypeFace t)    = TypeFace (f t)
mapValue f (TypeLangSet l) = TypeLangSet (f l)
mapValue f (TypeRange r)   = TypeRange (f r)

type Type = Value Proxy
type CType = CInt
 
marshalType :: Value f -> CType
marshalType TypeVoid{}    = #const FcTypeVoid
marshalType TypeInteger{} = #const FcTypeInteger
marshalType TypeDouble{}  = #const FcTypeDouble
marshalType TypeString{}  = #const FcTypeString
marshalType TypeBool{}    = #const FcTypeBool
marshalType TypeMatrix{}  = #const FcTypeMatrix
marshalType TypeCharSet{} = #const FcTypeCharSet
marshalType TypeFace{}    = #const FcTypeFTFace
marshalType TypeLangSet{} = #const FcTypeLangSet
marshalType TypeRange{}   = #const FcTypeRange

marshalMaybeType :: Maybe (Value f) -> CType
marshalMaybeType = maybe (#const FcTypeUnknown) marshalType

unmarshalMaybeType :: CInt -> Maybe Type
unmarshalMaybeType (#const FcTypeVoid)    = Just (TypeVoid Proxy)
unmarshalMaybeType (#const FcTypeInteger) = Just (TypeInteger Proxy)
unmarshalMaybeType (#const FcTypeDouble)  = Just (TypeDouble Proxy)
unmarshalMaybeType (#const FcTypeString)  = Just (TypeString Proxy)
unmarshalMaybeType (#const FcTypeBool)    = Just (TypeBool Proxy)
unmarshalMaybeType (#const FcTypeMatrix)  = Just (TypeMatrix Proxy)
unmarshalMaybeType (#const FcTypeCharSet) = Just (TypeCharSet Proxy)
unmarshalMaybeType (#const FcTypeFTFace)  = Just (TypeFace Proxy)
unmarshalMaybeType (#const FcTypeLangSet) = Just (TypeLangSet Proxy)
unmarshalMaybeType (#const FcTypeRange)   = Just (TypeRange Proxy)
unmarshalMaybeType _ = Nothing -- FcTypeUnknown or any unknown inhabitant of the enumeration from later versions of freetype

-- create values. this isn't a 'poke' as you need to deal with the value
-- so poking twice isn't always safe
marshalValue :: Ptr (Value Identity) -> Value Identity -> IO CValue
marshalValue r val = do
  (#poke FcType, type) (marshalType val)
  let poke_ :: Storable a => a -> IO ()
      poke_ = (#poke FcType, u) r
  case val of 
    TypeVoid v -> poke_ v
    TypeInteger i -> poke_ (fromIntegral i :: CInt)
    TypeDouble d -> poke_ d
    TypeString s -> newCString s >>= poke_ 
    TypeMatrix m -> newMatrix m >> poke_
    TypeCharSet t -> withCharSet t $ c_charSetCopy >>= poke_
    TypeFace c -> poke_ c
    TypeLangSet l -> withLangSet l $ c_langSetCopy >>= poke_
    TypeRange rng -> withRange rng $ c_rangeCopy >=> poke_

fromCInt :: CInt -> Int
fromCInt = fromIntegral

-- after this, you should c_valueDestroy the ptr
unmarshalMaybeValue :: Ptr (Value Identity) -> IO (Maybe (Value Identity))
unmarshalMaybeValue r = unmarshalMaybeType <$> (#peek FcType, type) r >>= traverse step where
  peek_ :: Storable a => IO (Ptr a)
  peek_ = (#peek FcType, u) r

  step TypeVoid{}    = TypeVoid . Identity <$> peek_
  step TypeInteger{} = TypeInteger . Identity . fromCInt <$> peek_
  step TypeDouble{}  = TypeDouble . Identity <$> peek_
  step TypeString{}  = TypeString . Identtiy <$> (peek_ >>= peekCString)
  step TypeBool{}    = TypeBool . Identity . toEnum . fromCInt <$> peek_
  step TypeMatrix{}  = TypeMatrix . Identity <$> (peek_ >>= peek)
  step TypeCharSet{} = TypeCharSet . Identity <$> (peek_ >>= makeCharSet)
  step TypeFace{}    = TypeFace . Identity <$> peek_
  step TypeRange{}   = TypeRange . Identity <$> (peek_ >>= makeRange)
  
--------------------------------------------------------------------------------
-- * Matrices
--------------------------------------------------------------------------------

data Matrix = Matrix
  { xx, xy, yx yy :: {-# unpack #-} !Double
  } deriving (Eq,Ord,Show,Read)

mul :: Matrix -> Matrix -> Matrix
mul (Matrix axx axy ayx ayy) (Matrix bxx bxy byx byy) = Matrix
  (axx * bxx + axy * byx)
  (axx * bxy + axy * byy)
  (ayx * bxx + ayy * byx)
  (ayx * bxy + ayy * byy)

identity :: Matrix
identity = Matrix 1 0 0 1

-- pure, unlike fontconfig, as in fontconfig the coordinate system is upside down
rotate :: Matrix -> Double -> Double -> Matrix
rotate = m `mul` Matrix c (-s) s c

scale :: Matrix -> Double -> Double -> Matrix
scale sx sy = m `mul` Matrix sx 0 0 sy

shear :: Matrix -> Double -> Double -> Matrix 
shear sh sv = m `mul` Matrix 1 sh sv 1

instance Storable Matrix where
  size = #size FcMatrix
  alignment _ = #align FcMatrix
  peek p = FcMatrix
    <$> (#peek FcMatrix, xx) p
    <*> (#peek FcMatrix, xy) p
    <*> (#peek FcMatrix, yx) p
    <*> (#peek FcMatrix, yy) p
  poke p (FcMatrix a b c d) = do
    (#poke FcMatrix, xx) p a 
    (#poke FcMatrix, xy) p b
    (#poke FcMatrix, yx) p c
    (#poke FcMatrix, yy) p d

withMatrix :: Matrix -> (Ptr Matrix -> IO r) -> IO r
withMatrix m = bracket (newMatrix m) c_matrixFree

-- analogous to newCString, but can be freed only with c_matrixFree
newMatrix :: Matrix -> IO (Ptr Matrix)
newMatrix m = do
  pm <- c_matrixCopy d_identityMatrix
  pm <$ poke pm m

