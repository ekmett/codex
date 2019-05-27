{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
{-# language CPP #-}

-- | ffi to the harfbuzz library
--
-- As an internal module, I don't consider this module as supported by the PVP. Be careful.
module Graphics.Harfbuzz.Internal
  ( Blob(..)
  , Buffer(..)
  , Tag
    ( Tag
    , TAG
    , TAG_NONE
    , TAG_MAX
    , TAG_MAX_SIGNED
    )
  , Script
    ( Script
    , SCRIPT_COMMON
    , SCRIPT_INHERITED
    , SCRIPT_UNKNOWN
    , SCRIPT_ARABIC
    , SCRIPT_ARMENIAN
    , SCRIPT_BENGALI
    , SCRIPT_CYRILLIC
    , SCRIPT_DEVANAGARI
    , SCRIPT_GEORGIAN
    , SCRIPT_GREEK
    , SCRIPT_GUJARATI
    , SCRIPT_GURMUKHI
    , SCRIPT_HANGUL
    , SCRIPT_HAN
    , SCRIPT_HEBREW
    , SCRIPT_HIRAGANA
    , SCRIPT_KANNADA
    , SCRIPT_KATAKANA
    , SCRIPT_LAO
    , SCRIPT_LATIN
    , SCRIPT_MALAYALAM
    , SCRIPT_ORIYA
    , SCRIPT_TAMIL
    , SCRIPT_TELUGU
    , SCRIPT_THAI
    , SCRIPT_TIBETAN
    , SCRIPT_BOPOMOFO
    , SCRIPT_BRAILLE
    , SCRIPT_CANADIAN_SYLLABICS
    , SCRIPT_CHEROKEE
    , SCRIPT_ETHIOPIC
    , SCRIPT_KHMER
    , SCRIPT_MONGOLIAN
    , SCRIPT_MYANMAR
    , SCRIPT_OGHAM
    , SCRIPT_RUNIC
    , SCRIPT_SINHALA
    , SCRIPT_SYRIAC
    , SCRIPT_THAANA
    , SCRIPT_YI
    , SCRIPT_DESERET
    , SCRIPT_GOTHIC
    , SCRIPT_OLD_ITALIC
    , SCRIPT_BUHID
    , SCRIPT_HANUNOO
    , SCRIPT_TAGALOG
    , SCRIPT_TAGBANWA
    , SCRIPT_CYPRIOT
    , SCRIPT_LIMBU
    , SCRIPT_LINEAR_B
    , SCRIPT_OSMANYA
    , SCRIPT_SHAVIAN
    , SCRIPT_TAI_LE
    , SCRIPT_UGARITIC
    , SCRIPT_BUGINESE
    , SCRIPT_COPTIC
    , SCRIPT_GLAGOLITIC
    , SCRIPT_KHAROSHTHI
    , SCRIPT_NEW_TAI_LUE
    , SCRIPT_OLD_PERSIAN
    , SCRIPT_SYLOTI_NAGRI
    , SCRIPT_TIFINAGH
    , SCRIPT_BALINESE
    , SCRIPT_CUNEIFORM
    , SCRIPT_NKO
    , SCRIPT_PHAGS_PA
    , SCRIPT_PHOENICIAN
    , SCRIPT_CARIAN
    , SCRIPT_CHAM
    , SCRIPT_KAYAH_LI
    , SCRIPT_LEPCHA
    , SCRIPT_LYCIAN
    , SCRIPT_LYDIAN
    , SCRIPT_OL_CHIKI
    , SCRIPT_REJANG
    , SCRIPT_SAURASHTRA
    , SCRIPT_SUNDANESE
    , SCRIPT_VAI
    , SCRIPT_AVESTAN
    , SCRIPT_BAMUM
    , SCRIPT_EGYPTIAN_HIEROGLYPHS
    , SCRIPT_IMPERIAL_ARAMAIC
    , SCRIPT_INSCRIPTIONAL_PAHLAVI
    , SCRIPT_INSCRIPTIONAL_PARTHIAN
    , SCRIPT_JAVANESE
    , SCRIPT_KAITHI
    , SCRIPT_LISU
    , SCRIPT_MEETEI_MAYEK
    , SCRIPT_OLD_SOUTH_ARABIAN
    , SCRIPT_OLD_TURKIC
    , SCRIPT_SAMARITAN
    , SCRIPT_TAI_THAM
    , SCRIPT_TAI_VIET
    , SCRIPT_BATAK
    , SCRIPT_BRAHMI
    , SCRIPT_MANDAIC
    , SCRIPT_CHAKMA
    , SCRIPT_MEROITIC_CURSIVE
    , SCRIPT_MEROITIC_HIEROGLYPHS
    , SCRIPT_MIAO
    , SCRIPT_SHARADA
    , SCRIPT_SORA_SOMPENG
    , SCRIPT_TAKRI
    , SCRIPT_BASSA_VAH
    , SCRIPT_CAUCASIAN_ALBANIAN
    , SCRIPT_DUPLOYAN
    , SCRIPT_ELBASAN
    , SCRIPT_GRANTHA
    , SCRIPT_KHOJKI
    , SCRIPT_KHUDAWADI
    , SCRIPT_LINEAR_A
    , SCRIPT_MAHAJANI
    , SCRIPT_MANICHAEAN
    , SCRIPT_MENDE_KIKAKUI
    , SCRIPT_MODI
    , SCRIPT_MRO
    , SCRIPT_NABATAEAN
    , SCRIPT_OLD_NORTH_ARABIAN
    , SCRIPT_OLD_PERMIC
    , SCRIPT_PAHAWH_HMONG
    , SCRIPT_PALMYRENE
    , SCRIPT_PAU_CIN_HAU
    , SCRIPT_PSALTER_PAHLAVI
    , SCRIPT_SIDDHAM
    , SCRIPT_TIRHUTA
    , SCRIPT_WARANG_CITI
    , SCRIPT_AHOM
    , SCRIPT_ANATOLIAN_HIEROGLYPHS
    , SCRIPT_HATRAN
    , SCRIPT_MULTANI
    , SCRIPT_OLD_HUNGARIAN
    , SCRIPT_SIGNWRITING
    , SCRIPT_ADLAM
    , SCRIPT_BHAIKSUKI
    , SCRIPT_MARCHEN
    , SCRIPT_OSAGE
    , SCRIPT_TANGUT
    , SCRIPT_NEWA
    , SCRIPT_MASARAM_GONDI
    , SCRIPT_NUSHU
    , SCRIPT_SOYOMBO
    , SCRIPT_ZANABAZAR_SQUARE
    , SCRIPT_DOGRA
    , SCRIPT_GUNJALA_GONDI
    , SCRIPT_HANIFI_ROHINGYA
    , SCRIPT_MAKASAR
    , SCRIPT_MEDEFAIDRIN
    , SCRIPT_OLD_SOGDIAN
    , SCRIPT_SOGDIAN
    , SCRIPT_ELYMAIC
    , SCRIPT_NANDINAGARI
    , SCRIPT_NYIAKENG_PUACHUE_HMONG
    , SCRIPT_WANCHO
    , SCRIPT_INVALID
    , SCRIPT__MAX_VALUE
    , SCRIPT__MAX_VALUE_SIGNED
    )
  , MemoryMode
    ( MemoryMode
    , MEMORY_MODE_DUPLICATE
    , MEMORY_MODE_READONLY
    , MEMORY_MODE_WRITABLE
    , MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE
    )
  , Direction
    ( Direction
    , DIRECTION_INVALID
    , DIRECTION_LTR
    , DIRECTION_RTL
    , DIRECTION_BTT
    , DIRECTION_TTB
    )
  -- * internals
  , withSelf
  , cbool
  , newByteStringCStringLen
  , harfbuzzCtx
  ) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as Strict
import Foreign
import Data.Coerce
import Data.Data (Data)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.String
import Foreign.C
import Foreign.Marshal.Unsafe
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import Text.Read

#include <hb.h>

newtype Blob = Blob { getBlob :: ForeignPtr Blob } deriving (Eq, Ord, Show, Data)

newtype Buffer = Buffer { getBuffer :: ForeignPtr Buffer } deriving (Eq, Ord, Show, Data)

newtype Tag = Tag Word32 deriving (Eq,Ord,Num,Enum,Real,Integral,Storable)
{-# complete Tag #-}
{-# complete TAG #-}

w2c :: Word32 -> Char
w2c = toEnum . fromIntegral

c2w :: Char -> Word32
c2w = fromIntegral . fromEnum

untag :: Tag -> (Char, Char, Char, Char)
untag (Tag w) =
  ( w2c (unsafeShiftR w 24 .&. 0xff)
  , w2c (unsafeShiftR w 16 .&. 0xff)
  , w2c (unsafeShiftR w 8 .&. 0xff)
  , w2c (w .&. 0xff)
  )

pattern TAG :: Char -> Char -> Char -> Char -> Tag
pattern TAG a b c d <- (untag -> (a,b,c,d)) where
  TAG a b c d = Tag $
    unsafeShiftL (c2w a .&. 0xff) 24 .|.
    unsafeShiftL (c2w b .&. 0xff) 16 .|.
    unsafeShiftL (c2w c .&. 0xff) 8  .|.
    (c2w d .&. 0xff)

pattern TAG_NONE :: Tag
pattern TAG_NONE = #const HB_TAG_NONE

pattern TAG_MAX:: Tag
pattern TAG_MAX = #const HB_TAG_MAX

pattern TAG_MAX_SIGNED :: Tag
pattern TAG_MAX_SIGNED = #const HB_TAG_MAX_SIGNED

instance Show Tag where
  showsPrec e (TAG a b c d) = showsPrec e [a,b,c,d]

instance Read Tag where
  readPrec = fromString <$> step readPrec

instance Default Tag where
  def = TAG_NONE

instance IsString Tag where
  fromString [] = TAG ' ' ' ' ' ' ' '
  fromString [a] = TAG a ' ' ' ' ' '
  fromString [a,b] = TAG a b ' ' ' '
  fromString [a,b,c] = TAG a b c ' '
  fromString (a:b:c:d:_) = TAG a b c d

newtype Script = Script Word32 deriving (Eq,Ord,Num,Enum,Real,Integral,Storable)

instance Show Script where
  showsPrec e (Script w) = showsPrec e (Tag w)

instance Read Script where
  readPrec = fromString <$> step readPrec

pattern SCRIPT_COMMON :: Script
pattern SCRIPT_INHERITED :: Script
pattern SCRIPT_UNKNOWN :: Script
pattern SCRIPT_ARABIC :: Script
pattern SCRIPT_ARMENIAN :: Script
pattern SCRIPT_BENGALI :: Script
pattern SCRIPT_CYRILLIC :: Script
pattern SCRIPT_DEVANAGARI :: Script
pattern SCRIPT_GEORGIAN :: Script
pattern SCRIPT_GREEK :: Script
pattern SCRIPT_GUJARATI :: Script
pattern SCRIPT_GURMUKHI :: Script
pattern SCRIPT_HANGUL :: Script
pattern SCRIPT_HAN :: Script
pattern SCRIPT_HEBREW :: Script
pattern SCRIPT_HIRAGANA :: Script
pattern SCRIPT_KANNADA :: Script
pattern SCRIPT_KATAKANA :: Script
pattern SCRIPT_LAO :: Script
pattern SCRIPT_LATIN :: Script
pattern SCRIPT_MALAYALAM :: Script
pattern SCRIPT_ORIYA :: Script
pattern SCRIPT_TAMIL :: Script
pattern SCRIPT_TELUGU :: Script
pattern SCRIPT_THAI :: Script
pattern SCRIPT_TIBETAN :: Script
pattern SCRIPT_BOPOMOFO :: Script
pattern SCRIPT_BRAILLE :: Script
pattern SCRIPT_CANADIAN_SYLLABICS :: Script
pattern SCRIPT_CHEROKEE :: Script
pattern SCRIPT_ETHIOPIC :: Script
pattern SCRIPT_KHMER :: Script
pattern SCRIPT_MONGOLIAN :: Script
pattern SCRIPT_MYANMAR :: Script
pattern SCRIPT_OGHAM :: Script
pattern SCRIPT_RUNIC :: Script
pattern SCRIPT_SINHALA :: Script
pattern SCRIPT_SYRIAC :: Script
pattern SCRIPT_THAANA :: Script
pattern SCRIPT_YI :: Script
pattern SCRIPT_DESERET :: Script
pattern SCRIPT_GOTHIC :: Script
pattern SCRIPT_OLD_ITALIC :: Script
pattern SCRIPT_BUHID :: Script
pattern SCRIPT_HANUNOO :: Script
pattern SCRIPT_TAGALOG :: Script
pattern SCRIPT_TAGBANWA :: Script
pattern SCRIPT_CYPRIOT :: Script
pattern SCRIPT_LIMBU :: Script
pattern SCRIPT_LINEAR_B :: Script
pattern SCRIPT_OSMANYA :: Script
pattern SCRIPT_SHAVIAN :: Script
pattern SCRIPT_TAI_LE :: Script
pattern SCRIPT_UGARITIC :: Script
pattern SCRIPT_BUGINESE :: Script
pattern SCRIPT_COPTIC :: Script
pattern SCRIPT_GLAGOLITIC :: Script
pattern SCRIPT_KHAROSHTHI :: Script
pattern SCRIPT_NEW_TAI_LUE :: Script
pattern SCRIPT_OLD_PERSIAN :: Script
pattern SCRIPT_SYLOTI_NAGRI :: Script
pattern SCRIPT_TIFINAGH :: Script
pattern SCRIPT_BALINESE :: Script
pattern SCRIPT_CUNEIFORM :: Script
pattern SCRIPT_NKO :: Script
pattern SCRIPT_PHAGS_PA :: Script
pattern SCRIPT_PHOENICIAN :: Script
pattern SCRIPT_CARIAN :: Script
pattern SCRIPT_CHAM :: Script
pattern SCRIPT_KAYAH_LI :: Script
pattern SCRIPT_LEPCHA :: Script
pattern SCRIPT_LYCIAN :: Script
pattern SCRIPT_LYDIAN :: Script
pattern SCRIPT_OL_CHIKI :: Script
pattern SCRIPT_REJANG :: Script
pattern SCRIPT_SAURASHTRA :: Script
pattern SCRIPT_SUNDANESE :: Script
pattern SCRIPT_VAI :: Script
pattern SCRIPT_AVESTAN :: Script
pattern SCRIPT_BAMUM :: Script
pattern SCRIPT_EGYPTIAN_HIEROGLYPHS :: Script
pattern SCRIPT_IMPERIAL_ARAMAIC :: Script
pattern SCRIPT_INSCRIPTIONAL_PAHLAVI :: Script
pattern SCRIPT_INSCRIPTIONAL_PARTHIAN :: Script
pattern SCRIPT_JAVANESE :: Script
pattern SCRIPT_KAITHI :: Script
pattern SCRIPT_LISU :: Script
pattern SCRIPT_MEETEI_MAYEK :: Script
pattern SCRIPT_OLD_SOUTH_ARABIAN :: Script
pattern SCRIPT_OLD_TURKIC :: Script
pattern SCRIPT_SAMARITAN :: Script
pattern SCRIPT_TAI_THAM :: Script
pattern SCRIPT_TAI_VIET :: Script
pattern SCRIPT_BATAK :: Script
pattern SCRIPT_BRAHMI :: Script
pattern SCRIPT_MANDAIC :: Script
pattern SCRIPT_CHAKMA :: Script
pattern SCRIPT_MEROITIC_CURSIVE :: Script
pattern SCRIPT_MEROITIC_HIEROGLYPHS :: Script
pattern SCRIPT_MIAO :: Script
pattern SCRIPT_SHARADA :: Script
pattern SCRIPT_SORA_SOMPENG :: Script
pattern SCRIPT_TAKRI :: Script
pattern SCRIPT_BASSA_VAH :: Script
pattern SCRIPT_CAUCASIAN_ALBANIAN :: Script
pattern SCRIPT_DUPLOYAN :: Script
pattern SCRIPT_ELBASAN :: Script
pattern SCRIPT_GRANTHA :: Script
pattern SCRIPT_KHOJKI :: Script
pattern SCRIPT_KHUDAWADI :: Script
pattern SCRIPT_LINEAR_A :: Script
pattern SCRIPT_MAHAJANI :: Script
pattern SCRIPT_MANICHAEAN :: Script
pattern SCRIPT_MENDE_KIKAKUI :: Script
pattern SCRIPT_MODI :: Script
pattern SCRIPT_MRO :: Script
pattern SCRIPT_NABATAEAN :: Script
pattern SCRIPT_OLD_NORTH_ARABIAN :: Script
pattern SCRIPT_OLD_PERMIC :: Script
pattern SCRIPT_PAHAWH_HMONG :: Script
pattern SCRIPT_PALMYRENE :: Script
pattern SCRIPT_PAU_CIN_HAU :: Script
pattern SCRIPT_PSALTER_PAHLAVI :: Script
pattern SCRIPT_SIDDHAM :: Script
pattern SCRIPT_TIRHUTA :: Script
pattern SCRIPT_WARANG_CITI :: Script
pattern SCRIPT_AHOM :: Script
pattern SCRIPT_ANATOLIAN_HIEROGLYPHS :: Script
pattern SCRIPT_HATRAN :: Script
pattern SCRIPT_MULTANI :: Script
pattern SCRIPT_OLD_HUNGARIAN :: Script
pattern SCRIPT_SIGNWRITING :: Script
pattern SCRIPT_ADLAM :: Script
pattern SCRIPT_BHAIKSUKI :: Script
pattern SCRIPT_MARCHEN :: Script
pattern SCRIPT_OSAGE :: Script
pattern SCRIPT_TANGUT :: Script
pattern SCRIPT_NEWA :: Script
pattern SCRIPT_MASARAM_GONDI :: Script
pattern SCRIPT_NUSHU :: Script
pattern SCRIPT_SOYOMBO :: Script
pattern SCRIPT_ZANABAZAR_SQUARE :: Script
pattern SCRIPT_DOGRA :: Script
pattern SCRIPT_GUNJALA_GONDI :: Script
pattern SCRIPT_HANIFI_ROHINGYA :: Script
pattern SCRIPT_MAKASAR :: Script
pattern SCRIPT_MEDEFAIDRIN :: Script
pattern SCRIPT_OLD_SOGDIAN :: Script
pattern SCRIPT_SOGDIAN :: Script
pattern SCRIPT_ELYMAIC :: Script
pattern SCRIPT_NANDINAGARI :: Script
pattern SCRIPT_NYIAKENG_PUACHUE_HMONG :: Script
pattern SCRIPT_WANCHO :: Script
pattern SCRIPT_INVALID :: Script
pattern SCRIPT__MAX_VALUE :: Script
pattern SCRIPT__MAX_VALUE_SIGNED :: Script

pattern SCRIPT_COMMON = #const HB_SCRIPT_COMMON
pattern SCRIPT_INHERITED = #const HB_SCRIPT_INHERITED
pattern SCRIPT_UNKNOWN = #const HB_SCRIPT_UNKNOWN
pattern SCRIPT_ARABIC = #const HB_SCRIPT_ARABIC
pattern SCRIPT_ARMENIAN = #const HB_SCRIPT_ARMENIAN
pattern SCRIPT_BENGALI = #const HB_SCRIPT_BENGALI
pattern SCRIPT_CYRILLIC = #const HB_SCRIPT_CYRILLIC
pattern SCRIPT_DEVANAGARI = #const HB_SCRIPT_DEVANAGARI
pattern SCRIPT_GEORGIAN = #const HB_SCRIPT_GEORGIAN
pattern SCRIPT_GREEK = #const HB_SCRIPT_GREEK
pattern SCRIPT_GUJARATI = #const HB_SCRIPT_GUJARATI
pattern SCRIPT_GURMUKHI = #const HB_SCRIPT_GURMUKHI
pattern SCRIPT_HANGUL = #const HB_SCRIPT_HANGUL
pattern SCRIPT_HAN = #const HB_SCRIPT_HAN
pattern SCRIPT_HEBREW = #const HB_SCRIPT_HEBREW
pattern SCRIPT_HIRAGANA = #const HB_SCRIPT_HIRAGANA
pattern SCRIPT_KANNADA = #const HB_SCRIPT_KANNADA
pattern SCRIPT_KATAKANA = #const HB_SCRIPT_KATAKANA
pattern SCRIPT_LAO = #const HB_SCRIPT_LAO
pattern SCRIPT_LATIN = #const HB_SCRIPT_LATIN
pattern SCRIPT_MALAYALAM = #const HB_SCRIPT_MALAYALAM
pattern SCRIPT_ORIYA = #const HB_SCRIPT_ORIYA
pattern SCRIPT_TAMIL = #const HB_SCRIPT_TAMIL
pattern SCRIPT_TELUGU = #const HB_SCRIPT_TELUGU
pattern SCRIPT_THAI = #const HB_SCRIPT_THAI
pattern SCRIPT_TIBETAN = #const HB_SCRIPT_TIBETAN
pattern SCRIPT_BOPOMOFO = #const HB_SCRIPT_BOPOMOFO
pattern SCRIPT_BRAILLE = #const HB_SCRIPT_BRAILLE
pattern SCRIPT_CANADIAN_SYLLABICS = #const HB_SCRIPT_CANADIAN_SYLLABICS
pattern SCRIPT_CHEROKEE = #const HB_SCRIPT_CHEROKEE
pattern SCRIPT_ETHIOPIC = #const HB_SCRIPT_ETHIOPIC
pattern SCRIPT_KHMER = #const HB_SCRIPT_KHMER
pattern SCRIPT_MONGOLIAN = #const HB_SCRIPT_MONGOLIAN
pattern SCRIPT_MYANMAR = #const HB_SCRIPT_MYANMAR
pattern SCRIPT_OGHAM = #const HB_SCRIPT_OGHAM
pattern SCRIPT_RUNIC = #const HB_SCRIPT_RUNIC
pattern SCRIPT_SINHALA = #const HB_SCRIPT_SINHALA
pattern SCRIPT_SYRIAC = #const HB_SCRIPT_SYRIAC
pattern SCRIPT_THAANA = #const HB_SCRIPT_THAANA
pattern SCRIPT_YI = #const HB_SCRIPT_YI
pattern SCRIPT_DESERET = #const HB_SCRIPT_DESERET
pattern SCRIPT_GOTHIC = #const HB_SCRIPT_GOTHIC
pattern SCRIPT_OLD_ITALIC = #const HB_SCRIPT_OLD_ITALIC
pattern SCRIPT_BUHID = #const HB_SCRIPT_BUHID
pattern SCRIPT_HANUNOO = #const HB_SCRIPT_HANUNOO
pattern SCRIPT_TAGALOG = #const HB_SCRIPT_TAGALOG
pattern SCRIPT_TAGBANWA = #const HB_SCRIPT_TAGBANWA
pattern SCRIPT_CYPRIOT = #const HB_SCRIPT_CYPRIOT
pattern SCRIPT_LIMBU = #const HB_SCRIPT_LIMBU
pattern SCRIPT_LINEAR_B = #const HB_SCRIPT_LINEAR_B
pattern SCRIPT_OSMANYA = #const HB_SCRIPT_OSMANYA
pattern SCRIPT_SHAVIAN = #const HB_SCRIPT_SHAVIAN
pattern SCRIPT_TAI_LE = #const HB_SCRIPT_TAI_LE
pattern SCRIPT_UGARITIC = #const HB_SCRIPT_UGARITIC
pattern SCRIPT_BUGINESE = #const HB_SCRIPT_BUGINESE
pattern SCRIPT_COPTIC = #const HB_SCRIPT_COPTIC
pattern SCRIPT_GLAGOLITIC = #const HB_SCRIPT_GLAGOLITIC
pattern SCRIPT_KHAROSHTHI = #const HB_SCRIPT_KHAROSHTHI
pattern SCRIPT_NEW_TAI_LUE = #const HB_SCRIPT_NEW_TAI_LUE
pattern SCRIPT_OLD_PERSIAN = #const HB_SCRIPT_OLD_PERSIAN
pattern SCRIPT_SYLOTI_NAGRI = #const HB_SCRIPT_SYLOTI_NAGRI
pattern SCRIPT_TIFINAGH = #const HB_SCRIPT_TIFINAGH
pattern SCRIPT_BALINESE = #const HB_SCRIPT_BALINESE
pattern SCRIPT_CUNEIFORM = #const HB_SCRIPT_CUNEIFORM
pattern SCRIPT_NKO = #const HB_SCRIPT_NKO
pattern SCRIPT_PHAGS_PA = #const HB_SCRIPT_PHAGS_PA
pattern SCRIPT_PHOENICIAN = #const HB_SCRIPT_PHOENICIAN
pattern SCRIPT_CARIAN = #const HB_SCRIPT_CARIAN
pattern SCRIPT_CHAM = #const HB_SCRIPT_CHAM
pattern SCRIPT_KAYAH_LI = #const HB_SCRIPT_KAYAH_LI
pattern SCRIPT_LEPCHA = #const HB_SCRIPT_LEPCHA
pattern SCRIPT_LYCIAN = #const HB_SCRIPT_LYCIAN
pattern SCRIPT_LYDIAN = #const HB_SCRIPT_LYDIAN
pattern SCRIPT_OL_CHIKI = #const HB_SCRIPT_OL_CHIKI
pattern SCRIPT_REJANG = #const HB_SCRIPT_REJANG
pattern SCRIPT_SAURASHTRA = #const HB_SCRIPT_SAURASHTRA
pattern SCRIPT_SUNDANESE = #const HB_SCRIPT_SUNDANESE
pattern SCRIPT_VAI = #const HB_SCRIPT_VAI
pattern SCRIPT_AVESTAN = #const HB_SCRIPT_AVESTAN
pattern SCRIPT_BAMUM = #const HB_SCRIPT_BAMUM
pattern SCRIPT_EGYPTIAN_HIEROGLYPHS = #const HB_SCRIPT_EGYPTIAN_HIEROGLYPHS
pattern SCRIPT_IMPERIAL_ARAMAIC = #const HB_SCRIPT_IMPERIAL_ARAMAIC
pattern SCRIPT_INSCRIPTIONAL_PAHLAVI = #const HB_SCRIPT_INSCRIPTIONAL_PAHLAVI
pattern SCRIPT_INSCRIPTIONAL_PARTHIAN = #const HB_SCRIPT_INSCRIPTIONAL_PARTHIAN
pattern SCRIPT_JAVANESE = #const HB_SCRIPT_JAVANESE
pattern SCRIPT_KAITHI = #const HB_SCRIPT_KAITHI
pattern SCRIPT_LISU = #const HB_SCRIPT_LISU
pattern SCRIPT_MEETEI_MAYEK = #const HB_SCRIPT_MEETEI_MAYEK
pattern SCRIPT_OLD_SOUTH_ARABIAN = #const HB_SCRIPT_OLD_SOUTH_ARABIAN
pattern SCRIPT_OLD_TURKIC = #const HB_SCRIPT_OLD_TURKIC
pattern SCRIPT_SAMARITAN = #const HB_SCRIPT_SAMARITAN
pattern SCRIPT_TAI_THAM = #const HB_SCRIPT_TAI_THAM
pattern SCRIPT_TAI_VIET = #const HB_SCRIPT_TAI_VIET
pattern SCRIPT_BATAK = #const HB_SCRIPT_BATAK
pattern SCRIPT_BRAHMI = #const HB_SCRIPT_BRAHMI
pattern SCRIPT_MANDAIC = #const HB_SCRIPT_MANDAIC
pattern SCRIPT_CHAKMA = #const HB_SCRIPT_CHAKMA
pattern SCRIPT_MEROITIC_CURSIVE = #const HB_SCRIPT_MEROITIC_CURSIVE
pattern SCRIPT_MEROITIC_HIEROGLYPHS = #const HB_SCRIPT_MEROITIC_HIEROGLYPHS
pattern SCRIPT_MIAO = #const HB_SCRIPT_MIAO
pattern SCRIPT_SHARADA = #const HB_SCRIPT_SHARADA
pattern SCRIPT_SORA_SOMPENG = #const HB_SCRIPT_SORA_SOMPENG
pattern SCRIPT_TAKRI = #const HB_SCRIPT_TAKRI
pattern SCRIPT_BASSA_VAH = #const HB_SCRIPT_BASSA_VAH
pattern SCRIPT_CAUCASIAN_ALBANIAN = #const HB_SCRIPT_CAUCASIAN_ALBANIAN
pattern SCRIPT_DUPLOYAN = #const HB_SCRIPT_DUPLOYAN
pattern SCRIPT_ELBASAN = #const HB_SCRIPT_ELBASAN
pattern SCRIPT_GRANTHA = #const HB_SCRIPT_GRANTHA
pattern SCRIPT_KHOJKI = #const HB_SCRIPT_KHOJKI
pattern SCRIPT_KHUDAWADI = #const HB_SCRIPT_KHUDAWADI
pattern SCRIPT_LINEAR_A = #const HB_SCRIPT_LINEAR_A
pattern SCRIPT_MAHAJANI = #const HB_SCRIPT_MAHAJANI
pattern SCRIPT_MANICHAEAN = #const HB_SCRIPT_MANICHAEAN
pattern SCRIPT_MENDE_KIKAKUI = #const HB_SCRIPT_MENDE_KIKAKUI
pattern SCRIPT_MODI = #const HB_SCRIPT_MODI
pattern SCRIPT_MRO = #const HB_SCRIPT_MRO
pattern SCRIPT_NABATAEAN = #const HB_SCRIPT_NABATAEAN
pattern SCRIPT_OLD_NORTH_ARABIAN = #const HB_SCRIPT_OLD_NORTH_ARABIAN
pattern SCRIPT_OLD_PERMIC = #const HB_SCRIPT_OLD_PERMIC
pattern SCRIPT_PAHAWH_HMONG = #const HB_SCRIPT_PAHAWH_HMONG
pattern SCRIPT_PALMYRENE = #const HB_SCRIPT_PALMYRENE
pattern SCRIPT_PAU_CIN_HAU = #const HB_SCRIPT_PAU_CIN_HAU
pattern SCRIPT_PSALTER_PAHLAVI = #const HB_SCRIPT_PSALTER_PAHLAVI
pattern SCRIPT_SIDDHAM = #const HB_SCRIPT_SIDDHAM
pattern SCRIPT_TIRHUTA = #const HB_SCRIPT_TIRHUTA
pattern SCRIPT_WARANG_CITI = #const HB_SCRIPT_WARANG_CITI
pattern SCRIPT_AHOM = #const HB_SCRIPT_AHOM
pattern SCRIPT_ANATOLIAN_HIEROGLYPHS = #const HB_SCRIPT_ANATOLIAN_HIEROGLYPHS
pattern SCRIPT_HATRAN = #const HB_SCRIPT_HATRAN
pattern SCRIPT_MULTANI = #const HB_SCRIPT_MULTANI
pattern SCRIPT_OLD_HUNGARIAN = #const HB_SCRIPT_OLD_HUNGARIAN
pattern SCRIPT_SIGNWRITING = #const HB_SCRIPT_SIGNWRITING
pattern SCRIPT_ADLAM = #const HB_SCRIPT_ADLAM
pattern SCRIPT_BHAIKSUKI = #const HB_SCRIPT_BHAIKSUKI
pattern SCRIPT_MARCHEN = #const HB_SCRIPT_MARCHEN
pattern SCRIPT_OSAGE = #const HB_SCRIPT_OSAGE
pattern SCRIPT_TANGUT = #const HB_SCRIPT_TANGUT
pattern SCRIPT_NEWA = #const HB_SCRIPT_NEWA
pattern SCRIPT_MASARAM_GONDI = #const HB_SCRIPT_MASARAM_GONDI
pattern SCRIPT_NUSHU = #const HB_SCRIPT_NUSHU
pattern SCRIPT_SOYOMBO = #const HB_SCRIPT_SOYOMBO
pattern SCRIPT_ZANABAZAR_SQUARE = #const HB_SCRIPT_ZANABAZAR_SQUARE
pattern SCRIPT_DOGRA = #const HB_SCRIPT_DOGRA
pattern SCRIPT_GUNJALA_GONDI = #const HB_SCRIPT_GUNJALA_GONDI
pattern SCRIPT_HANIFI_ROHINGYA = #const HB_SCRIPT_HANIFI_ROHINGYA
pattern SCRIPT_MAKASAR = #const HB_SCRIPT_MAKASAR
pattern SCRIPT_MEDEFAIDRIN = #const HB_SCRIPT_MEDEFAIDRIN
pattern SCRIPT_OLD_SOGDIAN = #const HB_SCRIPT_OLD_SOGDIAN
pattern SCRIPT_SOGDIAN = #const HB_SCRIPT_SOGDIAN
pattern SCRIPT_ELYMAIC = #const HB_SCRIPT_ELYMAIC
pattern SCRIPT_NANDINAGARI = #const HB_SCRIPT_NANDINAGARI
pattern SCRIPT_NYIAKENG_PUACHUE_HMONG = #const HB_SCRIPT_NYIAKENG_PUACHUE_HMONG
pattern SCRIPT_WANCHO = #const HB_SCRIPT_WANCHO
pattern SCRIPT_INVALID = #const HB_SCRIPT_INVALID
pattern SCRIPT__MAX_VALUE = #const _HB_SCRIPT_MAX_VALUE
pattern SCRIPT__MAX_VALUE_SIGNED = #const _HB_SCRIPT_MAX_VALUE_SIGNED

newtype Direction = Direction Word32  deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

pattern DIRECTION_INVALID :: Direction
pattern DIRECTION_LTR :: Direction
pattern DIRECTION_RTL :: Direction
pattern DIRECTION_TTB :: Direction
pattern DIRECTION_BTT :: Direction

pattern DIRECTION_INVALID = #const HB_DIRECTION_INVALID
pattern DIRECTION_LTR = #const HB_DIRECTION_LTR
pattern DIRECTION_RTL = #const HB_DIRECTION_RTL
pattern DIRECTION_TTB = #const HB_DIRECTION_TTB
pattern DIRECTION_BTT = #const HB_DIRECTION_BTT

newtype MemoryMode = MemoryMode CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

pattern MEMORY_MODE_DUPLICATE :: MemoryMode
pattern MEMORY_MODE_READONLY :: MemoryMode
pattern MEMORY_MODE_WRITABLE :: MemoryMode
pattern MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE :: MemoryMode

pattern MEMORY_MODE_DUPLICATE = #const HB_MEMORY_MODE_DUPLICATE
pattern MEMORY_MODE_READONLY = #const HB_MEMORY_MODE_READONLY
pattern MEMORY_MODE_WRITABLE = #const HB_MEMORY_MODE_WRITABLE
pattern MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE = #const HB_MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_blob_t", [t| Blob |])
    , (C.TypeName "hb_buffer_t", [t| Buffer |])
    , (C.TypeName "hb_tag_t", [t| Tag |])
    , (C.TypeName "hb_script_t", [t| Script |])
    , (C.TypeName "hb_direction_t", [t| Direction |])
    ]
  }

C.include "<hb.h>"

-- damnit ffi
instance IsString Script where
  fromString (fromString -> tag) = [C.pure|hb_script_t { hb_script_from_iso15924_tag($(hb_tag_t tag)) }|]

instance Default Blob where
  def = unsafeLocalState $ [C.exp|hb_blob_t * { hb_blob_get_empty() }|] >>= fmap Blob . newForeignPtr_ 
  {-# noinline def #-}

instance Default Buffer where
  def = unsafeLocalState $ [C.exp|hb_buffer_t * { hb_buffer_get_empty() }|] >>= fmap Buffer . newForeignPtr_ 
  {-# noinline def #-}


withSelf :: Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

cbool :: CInt -> Bool
cbool = toEnum . fromIntegral

-- | Copies 'ByteString' to newly allocated 'CString'. The result must be
-- | explicitly freed using 'free' or 'finalizerFree'.
newByteStringCStringLen :: Strict.ByteString -> IO CStringLen
newByteStringCStringLen (Strict.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    Strict.memcpy buf (p `plusPtr` o) l
    pokeByteOff buf l (0::Word8)
    return (castPtr buf, l)

-- * Inline C context

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ C.unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

anti :: C.Type C.CIdentifier -> TH.TypeQ -> TH.ExpQ -> C.SomeAntiQuoter
anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
  { C.aqParser = do
    hId <- C.parseIdentifier
    let cId = C.mangleHaskellIdentifier hId
    return (cId, cTy, hId)
  , C.aqMarshaller = \_purity _cTypes _cTy cId -> do
    hsTy <- [t| Ptr $hsTyQ |]
    hsExp <- getHsVariable "fontConfigCtx" cId
    hsExp' <- [| $w (coerce $(pure hsExp)) |]
    return (hsTy, hsExp')
  }

harfbuzzCtx :: C.Context
harfbuzzCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_blob_t", [t| Blob |])
    , (C.TypeName "hb_buffer_t", [t| Buffer |])
    , (C.TypeName "hb_bool_t", [t| CInt |])
    , (C.TypeName "hb_memory_mode_t", [t| MemoryMode |])
    , (C.TypeName "hb_tag_t", [t| Tag |])
    , (C.TypeName "hb_script_t", [t| Script |])
    , (C.TypeName "hb_direction_t", [t| Direction |])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr",        anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char (Just C.Unsigned)))) [t| CUChar |] [| withCUString |])
    , ("str",         anti (C.Ptr [C.CONST] (C.TypeSpecifier mempty (C.Char Nothing))) [t| CChar |] [| withCString |])
    , ("blob",        anti (ptr (C.TypeName "hb_blob_t")) [t| Blob |] [| withSelf |])
    , ("buffer",      anti (ptr (C.TypeName "hb_buffer_t")) [t| Buffer |] [| withSelf |])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
