{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Types
    ( IP(..)
      -- * IPv4
    , IPv4Addr
    , ipv4
    , ipv4ToChunks
    , parseIPv4
      -- * IPv6
    , IPv6Addr
    , ipv6
    , ipv6ToChunks
    , parseIPv6
      -- * Hostname
    , Hostname
      -- * PortNumber
    , PortNumber
    , portnumber
    ) where

import Data.Word
import Data.Bits
import Data.Char
import Data.ByteString (ByteString)
import Data.String
import Data.List (intercalate)
import Numeric (showHex, readHex)

data IP =
      IPv4 IPv4Addr
    | IPv6 IPv6Addr
  deriving (Eq, Ord)

instance Show IP where
    show (IPv4 ip) = show ip
    show (IPv6 ip) = show ip

instance Read IP where
    readsPrec _ s =
        case (parseIPv4 s, parseIPv6 s) of
            (Left _, Left _) -> []
            (Right (ip, r), Left _) -> [(IPv4 ip, r)]
            (Left _, Right (ip, r)) -> [(IPv6 ip, r)]
            (Right (ip1, r1), Right (ip2, r2)) -> [(IPv4 ip1, r1), (IPv6 ip2, r2)]

------------------------------------------------------------------------------
--                                   IPv4                                   --
------------------------------------------------------------------------------

-- | Internet Protocol Version 4 address a.b.c.d
newtype IPv4Addr = IPv4Addr Word32
    deriving (Eq, Ord)

instance Show IPv4Addr where
    show = printIPv4

instance Read IPv4Addr where
    readsPrec _ s =
        case parseIPv4 s of
            Left _    -> []
            Right ret -> [ret]

-- | Build an IPv4Addr from the given tuple
ipv4 :: (Word8, Word8, Word8, Word8) -- ^ IPv4 address (example: (192, 168, 0, 1))
     -> IPv4Addr
ipv4 (a,b,c,d) = IPv4Addr x
  where x = s 24 a .|. s 16 b .|. s 8 c .|. s 0 d
        s :: Int -> Word8 -> Word32
        s n w = fromIntegral w `shiftL` n

-- | Split an IPv4Addr into chunks
-- Example:
-- > ipv4ToChunk (ipv4 (192, 168, 0, 1)) = (192, 168, 0, 1)
ipv4ToChunks :: IPv4Addr
             -> (Word8, Word8, Word8, Word8)
ipv4ToChunks (IPv4Addr x) = (s 24 x, s 16 x, s 8 x, s 0 x)
 where s :: Int -> Word32 -> Word8
       s n w = fromIntegral (w `shiftR` n)

-- | Method to show an IPv4Addr. Use to make IPv4Addr an instance of Show
printIPv4 :: IPv4Addr -> String
printIPv4 ip =
  let (c1,c2,c3,c4) = ipv4ToChunks ip
   in intercalate "." $ map show [c1,c2,c3,c4]

-- | Method to read an IPv4Addr. Use to make IPv4Addr an instance of Read
parseIPv4 :: String -> Either String (IPv4Addr, String)
parseIPv4 s =
    case parseInt' (parseInt $ snd $ span isSeparator s) [] of
        Left err         -> Left err
        Right (addr, xs) -> Right (addr, xs)
  where
    parseInt' :: Either String (Word8, String)
              -> [Word8]
              -> Either String (IPv4Addr, String)
    parseInt' (Left err)           _          = Left err
    parseInt' (Right (w4, xs))     [w3,w2,w1] = Right (ipv4 (w1, w2, w3, w4), xs)
    parseInt' (Right (_,  []))     _          = Left "Not an ipv4 addr"
    parseInt' (Right (w,  '.':xs)) acc        = parseInt' (parseInt xs) (w:acc)
    parseInt' (Right (_,    c:_ )) _          = Left $ "Not an ipv4 addr: unexpected char '" ++ [c] ++ "'"

    parseInt :: String
             -> Either String (Word8, String)
    parseInt buf =
        case span isDigit buf of
            ([], x:_) -> Left $ "Not an ipv4 addr: unexpected char '" ++ [x] ++ "'"
            (l , xs)  -> Right (read l, xs)

------------------------------------------------------------------------------
--                                   IPv6                                   --
------------------------------------------------------------------------------

-- | Internal Protocol Version 6 address a:b:c:d:e:f:g:h
data IPv6Addr = IPv6Addr {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq, Ord)

instance Show IPv6Addr where
    show = printIPv6

instance Read IPv6Addr where
    readsPrec _ s =
        case parseIPv6 s of
            Left _    -> []
            Right ret -> [ret]

-- | Create an IPv6Addr from the given tuple
ipv6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
     -> IPv6Addr
ipv6 (a,b,c,d,e,f,g,h) = IPv6Addr x y
  where x = s 48 a .|. s 32 b .|. s 16 c .|. s 0 d
        y = s 48 e .|. s 32 f .|. s 16 g .|. s 0 h
        s :: Int -> Word16 -> Word64
        s n w = fromIntegral w `shiftL` n

-- | Split the IPv6Addr into chunks
-- Example:
-- > ipv6ToChunks . ipv6 (0a, 1e, 3, 11, 32, 7f, 74, 00) = (0a, 1e, 3, 11, 32, 7f, 74, 00)
ipv6ToChunks :: IPv6Addr
             -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
ipv6ToChunks (IPv6Addr a b) =
  (shiftToW16 48 a, shiftToW16 32 a, shiftToW16 16 a, shiftToW16 0 a
  ,shiftToW16 48 b, shiftToW16 32 b, shiftToW16 16 b, shiftToW16 0 b)
  where shiftToW16 :: Int -> Word64 -> Word16
        shiftToW16 n w = fromIntegral (w `shiftR` n)

-- | Print an IPv6Addr. Use to make IPv6Addr an instance of Show.
printIPv6 :: IPv6Addr -> String
printIPv6 ip =
  let (c1,c2,c3,c4,c5,c6,c7,c8) = ipv6ToChunks ip
   in intercalate ":" $ map showHex4 [c1,c2,c3,c4,c5,c6,c7,c8]
  where showHex4 c
            | c < 0x10   = '0':'0':'0':showHex c ""
            | c < 0x100  = '0':'0':showHex c ""
            | c < 0x1000 = '0':showHex c ""
            | otherwise  = showHex c ""

parseIPv6 :: String -> Either String (IPv6Addr, String)
parseIPv6 s =
    case parseInt' (parseHex $ snd $ span isSeparator s) [] of
        Left err         -> Left err
        Right (addr, xs) -> Right (addr, xs)
  where
    parseInt' :: Either String (Word16, String)
              -> [Word16]
              -> Either String (IPv6Addr, String)
    parseInt' (Left err)           _          = Left err
    parseInt' (Right (w8, xs))     [w7,w6,w5,w4,w3,w2,w1] = Right (ipv6 (w1, w2, w3, w4, w5, w6, w7, w8), xs)
    parseInt' (Right (_,  []))     _          = Left "Not an ipv6 addr"
    parseInt' (Right (w,  ':':xs)) acc        = parseInt' (parseHex xs) (w:acc)
    parseInt' (Right (_,    c:_ )) _          = Left $ "Not an ipv6 addr: unexpected char '" ++ [c] ++ "'"

    parseHex :: String
             -> Either String (Word16, String)
    parseHex buf =
        case span isHexDigit buf of
            ([], x:xs) -> case x of
                            ':' -> Right (0, x:xs)
                            _   -> Left $ "Not an ipv6 addr: unexpected char '" ++ [x] ++ "'"
            (l , xs)   -> let lhs = readHex l :: [(Word16, String)]
                          in  case lhs of
                                [(w, [])] -> Right (w, xs)
                                _  -> Left "can't fall here"

------------------------------------------------------------------------------
--                                 Hostname                                 --
------------------------------------------------------------------------------

newtype Hostname = Hostname ByteString
    deriving (Eq, Ord)

instance IsString Hostname where
    fromString s = hostname s

hostname :: String -> Hostname
hostname = undefined
    -- fixme : convert to punycode

------------------------------------------------------------------------------
--                                 PortNumber                               --
------------------------------------------------------------------------------

newtype PortNumber = PortNumber Word16
    deriving (Eq, Ord, Enum, Num, Real, Integral)

portnumber :: Int -> PortNumber
portnumber = PortNumber . fromIntegral

instance Show PortNumber where
    show (PortNumber p) = show p

instance Read PortNumber where
    readsPrec n s = map (\(v, r) -> (PortNumber v, r)) $ readsPrec n s

------------------------------------------------------------------------------
--                                  Service                                 --
------------------------------------------------------------------------------


