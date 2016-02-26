{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The core datatypes ('U256', 'S256', 'Word8', 'Address') and hex parsing.
module Masala.Word
    (
     U256 (..)
    ,S256 (..)
    ,U8 (..)
    ,Address (..)
    ,module Data.Bits
    ,showHex,showHexPad,showHexs,showBinary
    ,readHex,readHexs,hexSize
    ,u256ToU8s,u8sToU256s
    ,parseJSONHex
     ) where

import Data.Word
import Data.DoubleWord
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Data.Bits
import qualified Numeric as N
import qualified Data.Text as T
import Data.Char (intToDigit)
import Data.List.Split


-- | Main type, unsigned 256-bit word.
newtype U256 = U256 Word256 deriving (Num,Eq,Ord,Bounded,Enum,Integral,Real,Generic,Bits,Read,FiniteBits)
instance Show U256 where show (U256 u) = showHex u
instance FromJSON U256 where
    parseJSON = parseJSONHex "U256"


-- | Signed equivalent of 'U256'. Internal to VM only (no JSON support).
newtype S256 = S256 Int256 deriving (Num,Eq,Show,Ord,Bounded,Enum,Integral,Real,Generic,Bits)

-- | Newtype over Word8 to get hex output, mainly
newtype U8 = U8 Word8 deriving (Num,Eq,Ord,Bounded,Enum,Integral,Real,Generic,Bits,Read,FiniteBits)
instance Show U8 where show (U8 u) = showHex u


newtype Address = Address Word160 deriving (Num,Eq,Ord,Bounded,Enum,Integral,Real,Generic)
instance Show Address where show (Address u) = showHex u

instance FromJSON Address where parseJSON = parseJSONHex "Address"



showHex :: (Integral a, Show a) => a -> String
showHex a = N.showHex a ""

-- | showHex with padded 0s to hex size of type.
showHexPad :: (FiniteBits a, Integral a, Show a, Eq a, Num a) => a -> String
showHexPad v = pad $ showHex v where
    pad s = take (max 0 (hexSize v - length s)) (repeat '0') ++ s

hexSize :: FiniteBits b => b -> Int
hexSize v = finiteBitSize v `div` 4

-- | concat hex reps
showHexs :: (FiniteBits a, Integral a, Show a, Eq a, Num a) => [a] -> String
showHexs = concatMap showHexPad


-- | Binary rep.
showBinary :: (Show a, Integral a) => a -> String
showBinary i = N.showIntAtBase 2 intToDigit i ""

-- | JSON hexadecimal parser utility.
parseJSONHex :: (Eq a,Num a) => String -> Value -> Parser a
parseJSONHex name = withText name (either fail return . readHex . T.unpack)

-- | read unsigned hex in Either, supporting leading "0x"
readHex :: (Eq a,Num a) => String -> Either String a
readHex = ph . drop0x where
    ph s = case N.readHex s of
             [(a,"")] -> Right a
             _ -> Left $ "Invalid hex value " ++ s
    drop0x ('0':'x':a) = a
    drop0x a = a


-- | read hex values, must align with hex size
readHexs :: forall a . (Num a, Eq a, FiniteBits a, Bounded a) => String -> Either String [a]
readHexs s = if rm == 0
             then sequence $ map readHex $ chunksOf sz s
             else Left $ "readHexs: string must align on target size (" ++ show sz ++ "): " ++ s
    where sz = hexSize (minBound :: a)
          rm = length s `mod` sz


u8sToU256s :: [U8] -> [U256]
u8sToU256s = fst. foldr acc ([0],0)
    where acc v (t:ts,p) | p < 256 = (t + shift (fromIntegral v) p:ts, p + 8)
                         | otherwise = acc v (0:t:ts,0)
          acc _ ([],_) = error "c'est impossible"


u256ToU8s :: U256 -> [U8]
u256ToU8s 0 = [0]
u256ToU8s u = w8 [] u
    where w8 ws v | v > 0 = w8 (fromIntegral (v .&. 0xff):ws) (v `shiftR` 8)
                  | otherwise = ws
