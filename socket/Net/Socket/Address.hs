-- |
-- Module      : Net.Socket.Address
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Net.Socket.Address
    ( SockAddr(..)
    , SocketFamily
    -- * get
    , SockAddrReader
    , expect
    , getSockAdddrCommon
    , get8
    , getN32
    -- * put
    , SockAddrWriter
    , putSockAdddrCommon
    , put8
    , putN32
    ) where

import Data.Word
import Control.Applicative
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

newtype SockAddrReader a = SockAddrReader { runSockAddrReader :: Ptr Word8 -> Word32 -> IO a }
    deriving (Functor,Applicative,Monad)

newtype SockAddrWriter a = SockAddrWriter { runSockAddrWriter :: Ptr Word8 -> Word32 -> IO a }
    deriving (Functor,Applicative,Monad)

putSockAddrCommon :: Word8 -> Family -> SockAddrWriter
putSockAddrCommon len family =
    undefined

getSockAddrCommon :: SockAddrReader (Word8, Family)
getSockAddrCommon =
    undefined

put8 :: Word8 -> SockAddrWriter ()
put8 w = run $ \ptr n ->
    undefined

putN32 :: Word32 -> SockAddrWriter ()
putN32 w = run $ \ptr n ->
    if n < 4
        then error "n32"
        else undefined

--allocate ?

-- | Pre-define how much bytes are expected for reading the SockAddr
--
-- if the number is less than the expected value
-- then the call will abort the SockAddr reading.
expect :: Word32 -> SockAddrReader ()
expect = undefined

get8   = undefined
getN32 = undefined
