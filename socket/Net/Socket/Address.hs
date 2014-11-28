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
    , SocketFamily(..)
    -- * get
    , SockAddrReader
    , expect
    , sockAddrReaderError
    , getSockAddrCommon
    , get8
    , getN16
    , getN32
    -- * put
    , SockAddrWriter
    , sockAddrWriterError
    , putSockAddrCommon
    , put8
    , putN16
    , putN32
    ) where

import Data.Bits (shiftR, shiftL, (.|.))
import Data.Word
import Control.Applicative
import Control.Monad
import Foreign.Ptr
import Foreign.Storable

-- | A wrapper for the domain of the socket
-- e.g. AF_INET, AF_INET6, 
newtype SocketFamily = SocketFamily Word8 -- FIXME
    deriving (Show,Eq)

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

putSockAddrCommon :: Word8 -> SocketFamily -> SockAddrWriter ()
putSockAddrCommon len (SocketFamily family) = do
    writerEnsure 2
    putByte len
    putByte family

getSockAddrCommon :: SockAddrReader (Word8, SocketFamily)
getSockAddrCommon = do
    readerEnsure 2
    len    <- getByte
    family <- getByte
    return (len, SocketFamily $ fromIntegral family)

get8 :: SockAddrReader Word8
get8 = readerEnsure 1 >> getByte

getN16 :: SockAddrReader Word16
getN16 = do
    readerEnsure 4
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

--allocate ?

-- | Pre-define how much bytes are expected for reading the SockAddr
--
-- if the number is less than the expected value
-- then the call will abort the SockAddr reading.
expect :: Word32 -> SockAddrReader ()
expect n = undefined
