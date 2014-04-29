module Net.Types
    ( IPv4Addr
    , IPv6Addr
    , Hostname
    ) where

import Data.Int
import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import Data.String
import Data.List (intercalate)
import Numeric (showHex)

-- | Internet Protocol Version 4 address a.b.c.d
newtype IPv4Addr = IPv4Addr Word32
    deriving (Eq,Ord)

instance Show IPv4Addr where
    show = printIPv4

-- | Internal Protocol Version 6 address a:b:c:d:e:f:g:h
data IPv6Addr = IPv6Addr {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq,Ord)

instance Show IPv6Addr where
    show = printIPv6

data Hostname = Hostname ByteString
    deriving (Eq,Ord)

instance IsString Hostname where
    fromString s = toPunycode s

ipv6ToChunks :: IPv6Addr -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
ipv6ToChunks (IPv6Addr a b) =
  (shiftToW16 48 a, shiftToW16 32 a, shiftToW16 16 a, shiftToW16 0 a
  ,shiftToW16 48 b, shiftToW16 32 b,shiftToW16 16 b, shiftToW16 0 b)
  where shiftToW16 :: Int -> Word64 -> Word16
        shiftToW16 n w = fromIntegral (w `shiftR` n)

ipv6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> IPv6Addr
ipv6 (a,b,c,d,e,f,g,h) = IPv6Addr x y
  where x = s 48 a .|. s 32 b .|. s 16 c .|. s 0 d
        y = s 48 e .|. s 32 f .|. s 16 g .|. s 0 h
        s :: Int -> Word16 -> Word64
        s n w = fromIntegral w `shiftL` n

ipv4ToChunks :: IPv4Addr -> (Word8, Word8, Word8, Word8)
ipv4ToChunks (IPv4Addr x) = (s 24 x, s 16 x, s 8 x, s 0 x)
 where s :: Int -> Word32 -> Word8
       s n w = fromIntegral (w `shiftR` n)

ipv4 :: (Word8, Word8, Word8, Word8) -> IPv4Addr
ipv4 (a,b,c,d) = IPv4Addr x
  where x = s 24 a .|. s 16 b .|. s 8 c .|. s 0 d
        s :: Int -> Word8 -> Word32
        s n w = fromIntegral w `shiftL` n

printIPv4 :: IPv4Addr -> String
printIPv4 ip =
  let (c1,c2,c3,c4) = ipv4ToChunks ip
   in intercalate "." $ map show [c1,c2,c3,c4]

printIPv6 ip =
  let (c1,c2,c3,c4,c5,c6,c7,c8) = ipv6ToChunks ip
   in intercalate ":" $ map showHex4 [c1,c2,c3,c4,c5,c6,c7,c8]
  where showHex4 c
            | c < 0x10     = '0':'0':'0':showHex c ""
            | c < 0x100   = '0':'0':showHex c ""
            | c < 0x1000 = '0':showHex c ""
            | otherwise   = showHex c ""

parseIPv4 :: String -> Either String (IPv4Addr, String)
parseIPv4 s = undefined

parseIPv6 :: String -> Either String (IPv6Addr, String)
parseIPv6 s = undefined

toPunycode = undefined

{-
data PortNumber
data Service
-}
