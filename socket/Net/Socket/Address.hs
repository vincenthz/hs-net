{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Net.Socket.Address
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Net.Socket.Address
    ( SockAddr(..)
    , SocketAddrRaw(..)
    , SocketFamily
    , packFamily
    , unpackFamily
    , socketFamilyInet
    , socketFamilyInet6
    , socketFamilyUnix
    -- * get
    , SockAddrReader
    , runSockAddrReader
    , expect
    , sockAddrReaderError
    , get8
    , getN16
    , getN32
    , peekFamily
    , getFamily
    , getIPv4
    , getIPv6
    -- * put
    , SockAddrWriter
    , runSockAddrWriter
    , sockAddrWriterError
    , put8
    , putN16
    , putN32
    , putFamily
    , putIPv4
    , putIPv6
    ) where

import Data.Bits (shiftR, shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import Data.Word
import Control.Applicative
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Net.Socket.System.Internal
import Net.Types

-- | This is the in-memory representation of a 'struct sockaddr'
--
-- * The SockAddr has been marshalled (See SockAddr)
-- * The memory is ready to be given to the low level functions
newtype SocketAddrRaw = SocketAddrRaw ByteString

-- | Define types that can be used as socket address.
--
-- The definition need to be able to marshall and unmarshall
-- from the operating system dependant structure.
class SockAddr a where
    sockAddrToData   :: a -> SockAddrWriter ()
    sockAddrFromData :: SockAddrReader a
    sockAddrToParams :: a -> SocketFamily

type Memory = (Ptr Word8, Word32)

newtype SockAddrReader a = SockAddrReader { runSockAddrReader :: Memory -> IO (a,Memory) }
instance Functor SockAddrReader where
    fmap f m = SockAddrReader $ \mem -> runSockAddrReader m mem >>= \(a, mem2) -> return (f a, mem2)
instance Applicative SockAddrReader where
    pure = return
    (<*>) = ap
instance Monad SockAddrReader where
    return v  = SockAddrReader $ \mem -> return (v, mem)
    m1 >>= m2 = SockAddrReader $ \mem -> runSockAddrReader m1 mem >>= \(a, mem2) -> runSockAddrReader (m2 a) mem2

newtype SockAddrWriter a = SockAddrWriter { runSockAddrWriter :: Memory -> IO (a,Memory) }
instance Functor SockAddrWriter where
    fmap f m = SockAddrWriter $ \mem -> runSockAddrWriter m mem >>= \(a, mem2) -> return (f a, mem2)
instance Applicative SockAddrWriter where
    pure = return
    (<*>) = ap
instance Monad SockAddrWriter where
    return v  = SockAddrWriter $ \mem -> return (v, mem)
    m1 >>= m2 = SockAddrWriter $ \mem -> runSockAddrWriter m1 mem >>= \(a, mem2) -> runSockAddrWriter (m2 a) mem2

-- FIXME replace writer and reader error by a synchronous error propagation

writerEnsure :: Word32 -> SockAddrWriter ()
writerEnsure n = SockAddrWriter $ \(ptr,left) ->
    if left < n
        then error ("writer out of bound: requesting " ++ show n ++ " bytes but have " ++ show left ++ " bytes")
        else return ((), (ptr, left))

readerEnsure :: Word32 -> SockAddrReader ()
readerEnsure n = SockAddrReader $ \(ptr,left) ->
    if left < n
        then error ("reader out of bound: requesting " ++ show n ++ " bytes but have " ++ show left ++ " bytes")
        else return ((), (ptr, left))

sockAddrReaderError :: String -> SockAddrReader ()
sockAddrReaderError s = error s

sockAddrWriterError :: String -> SockAddrWriter ()
sockAddrWriterError s = error s

getByte :: SockAddrReader Word8
getByte = SockAddrReader $ \(ptr, n) ->
    peek ptr >>= \w -> return (w, (ptr `plusPtr` 1, n-1))

putByte :: Word8 -> SockAddrWriter ()
putByte w = SockAddrWriter $ \(ptr,n) ->
    poke ptr w >> return ((), (ptr `plusPtr` 1, n-1))

get8 :: SockAddrReader Word8
get8 = readerEnsure 1 >> getByte

getN16 :: SockAddrReader Word16
getN16 = do
    readerEnsure 2
    toN16 <$> getByte <*> getByte
  where toN16 c d = (fromIntegral c `shiftL` 8)  .|.  fromIntegral d

getN32 :: SockAddrReader Word32
getN32 = do
    readerEnsure 4
    toN32 <$> getByte <*> getByte <*> getByte <*> getByte
  where toN32 a b c d =
            (fromIntegral a `shiftL` 24) .|.
            (fromIntegral b `shiftL` 16) .|.
            (fromIntegral c `shiftL` 8)  .|.
            fromIntegral d


put8 :: Word8 -> SockAddrWriter ()
put8 w = writerEnsure 1 >> putByte w

putN16 :: Word16 -> SockAddrWriter ()
putN16 w = writerEnsure 2 >> putByte c >> putByte d
  where c = fromIntegral (w `shiftR` 8)
        d = fromIntegral w

putN32 :: Word32 -> SockAddrWriter ()
putN32 w = writerEnsure 4 >> putByte a >> putByte b >> putByte c >> putByte d
  where a = fromIntegral (w `shiftR` 24)
        b = fromIntegral (w `shiftR` 16)
        c = fromIntegral (w `shiftR` 8)
        d = fromIntegral w

-- | This function is use to extract the SocketFamily from the
-- SockAddrRaw.
peekFamily :: SocketAddrRaw
           -> IO SocketFamily
peekFamily (SocketAddrRaw bs)
    -- In the case the length of the SockAddrRaw is smaller than 14
    -- that means we might have made a mistake in the Marshalling of
    -- the SockAddr or the SockAddrRaw that has been returned from
    -- accept or another function was wrong.
    --
    -- TODO: throw an appropriate Exception
    | len < 14  = error "Net.Socket.Address.peekFamily: the given SockAddrRaw is smaller than the smallest size (14 bytes)"
    | otherwise =
        withForeignPtr fptr $ \ptr ->
            fst <$> runSockAddrReader readerFunction (ptr `plusPtr` off, fromIntegral len)
  where
    (fptr, off, len) = B.toForeignPtr bs
    readerFunction :: SockAddrReader SocketFamily
    readerFunction = do
        -- TODO: compare the size with the expected size
        -- * inet: 14
        -- * inet6: 28
        -- * ...
        _ <- get8
        getFamily

putFamily :: SocketFamily -> SockAddrWriter ()
putFamily = put8 . fromIntegral . packFamily

getFamily :: SockAddrReader SocketFamily
getFamily = unpackFamily . fromIntegral <$> get8

putIPv4 :: IPv4Addr -> SockAddrWriter ()
putIPv4 addr = put8 w1 >> put8 w2 >> put8 w3 >> put8 w4
  where
    (w1, w2, w3, w4) = ipv4ToChunks addr

getIPv4 :: SockAddrReader IPv4Addr
getIPv4 = ipv4 <$> ((,,,) <$> get8 <*> get8 <*> get8 <*> get8)

putIPv6 :: IPv6Addr -> SockAddrWriter ()
putIPv6 addr = do
    putN16 a >> putN16 b >> putN16 c >> putN16 d
    putN16 e >> putN16 f >> putN16 g >> putN16 h
  where
    (a,b,c,d,e,f,g,h) = ipv6ToChunks addr

getIPv6 :: SockAddrReader IPv6Addr
getIPv6 =
    ipv6 <$> ((,,,,,,,) <$> getN16 <*> getN16 <*> getN16 <*> getN16 <*> getN16 <*> getN16 <*> getN16 <*> getN16)

--allocate ?

-- | Pre-define how much bytes are expected for reading the SockAddr
--
-- if the number is less than the expected value
-- then the call will abort the SockAddr reading.
expect :: Word32 -> SockAddrReader ()
expect n = undefined
