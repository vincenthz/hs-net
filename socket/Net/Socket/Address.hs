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
    , getSockAddrCommon
    , get8
    , getN32
    -- * put
    , SockAddrWriter
    , putSockAddrCommon
    , put8
    , putN32
    ) where

import Data.Word
import Control.Applicative
import Control.Monad
import Foreign.Ptr

-- | A wrapper for the domain of the socket
-- e.g. AF_INET, AF_INET6, 
newtype SocketFamily = SocketFamily Int -- FIXME

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
    fmap f m = SockAddrReader $ \mem -> runSockAddrReader m mem >>= \(a, mem) -> return (f a, mem)
instance Applicative SockAddrReader where
    pure = return
    (<*>) = ap
instance Monad SockAddrReader where
    return v  = SockAddrReader $ \mem -> return (v, mem)
    m1 >>= m2 = SockAddrReader $ \mem -> runSockAddrReader m1 mem >>= \(a, mem2) -> runSockAddrReader (m2 a) mem2

newtype SockAddrWriter a = SockAddrWriter { runSockAddrWriter :: Memory -> IO (a,Memory) }

instance Functor SockAddrWriter where
    fmap f m = SockAddrWriter $ \mem -> runSockAddrWriter m mem >>= \(a, mem) -> return (f a, mem)
instance Applicative SockAddrWriter where
    pure = return
    (<*>) = ap
instance Monad SockAddrWriter where
    return v  = SockAddrWriter $ \mem -> return (v, mem)
    m1 >>= m2 = SockAddrWriter $ \mem -> runSockAddrWriter m1 mem >>= \(a, mem2) -> runSockAddrWriter (m2 a) mem2

putSockAddrCommon :: Word8 -> SocketFamily -> SockAddrWriter ()
putSockAddrCommon len family =
    undefined

getSockAddrCommon :: SockAddrReader (Word8, SocketFamily)
getSockAddrCommon =
    undefined

put8 :: Word8 -> SockAddrWriter ()
put8 w = SockAddrWriter $ \(ptr,n) ->
    undefined

putN32 :: Word32 -> SockAddrWriter ()
putN32 w = SockAddrWriter $ \(ptr,n) ->
    if n < 4
        then error "n32"
        else undefined

--allocate ?

-- | Pre-define how much bytes are expected for reading the SockAddr
--
-- if the number is less than the expected value
-- then the call will abort the SockAddr reading.
expect :: Word32 -> SockAddrReader ()
expect n = undefined

get8   = undefined
getN32 = undefined
