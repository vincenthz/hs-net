-- |
-- Module      : Net.Socket.System.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Net.Socket.System.Internal
    ( SocketMsgFlags(..)
    , CSockLen(..)
    , MsgHdr(..)
    , IOVec(..)
    , SocketType(..)
    , packSocketType
    , unpackSocketType

    , SocketFamily
    , packFamily
    , unpackFamily
    , socketFamilyInet
    , socketFamilyInet6
    , socketFamilyUnix

    , sockAddrSize
    ) where

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <sys/uio.h>

import Control.Applicative
import Control.Exception (throw)
import Data.Bits
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr

import Net.Socket.System.Error

-- | Socket Types.
--
-- The existence of a constructor does not necessarily imply that that
-- socket type is supported on your system: see 'isSupportedSocketType'.
data SocketType
        = NoSocketType -- ^ 0, used in getAddrInfo hints, for example
        | Stream -- ^ SOCK_STREAM
        | Datagram -- ^ SOCK_DGRAM
        | Raw -- ^ SOCK_RAW
        | RDM -- ^ SOCK_RDM
        | SeqPacket -- ^ SOCK_SEQPACKET
        deriving (Eq, Ord, Read, Show)

packSocketTypeRaw :: SocketType -> Maybe CInt
packSocketTypeRaw stype = case Just stype of
    -- the Just above is to disable GHC overlapping pattern
    -- detection: see comments for packSocketOption
    Just NoSocketType -> Just 0
#ifdef SOCK_STREAM
    Just Stream -> Just #const SOCK_STREAM
#endif
#ifdef SOCK_DGRAM
    Just Datagram -> Just #const SOCK_DGRAM
#endif
#ifdef SOCK_RAW
    Just Raw -> Just #const SOCK_RAW
#endif
#ifdef SOCK_RDM
    Just RDM -> Just #const SOCK_RDM
#endif
#ifdef SOCK_SEQPACKET
    Just SeqPacket -> Just #const SOCK_SEQPACKET
#endif
    _ -> Nothing

packSocketType :: SocketType -> CInt
packSocketType stype =
    case packSocketTypeRaw stype of
        Nothing -> throw $ SocketTypeNotAvailable (show stype)
        Just v  -> v

unpackSocketTypeRaw :: CInt -> Maybe SocketType
unpackSocketTypeRaw t = case t of
        0 -> Just NoSocketType
#ifdef SOCK_STREAM
        (#const SOCK_STREAM) -> Just Stream
#endif
#ifdef SOCK_DGRAM
        (#const SOCK_DGRAM) -> Just Datagram
#endif
#ifdef SOCK_RAW
        (#const SOCK_RAW) -> Just Raw
#endif
#ifdef SOCK_RDM
        (#const SOCK_RDM) -> Just RDM
#endif
#ifdef SOCK_SEQPACKET
        (#const SOCK_SEQPACKET) -> Just SeqPacket
#endif
        _ -> Nothing

-- | Try unpackSocketType on the CInt
unpackSocketType :: CInt -> SocketType
unpackSocketType ty =
    case unpackSocketTypeRaw ty of
        Nothing -> throw $ SocketTypeNotAvailable (show ty)
        Just v  -> v

newtype SocketFamily = SocketFamily CInt
  deriving (Show, Eq)

socketFamilyInet :: SocketFamily
socketFamilyInet =
#ifdef AF_INET
    SocketFamily (#const AF_INET)
#else
    throw (SocketFamilyNotAvailable "AF_INET")
#endif

socketFamilyInet6 :: SocketFamily
socketFamilyInet6 =
#ifdef AF_INET6
    SocketFamily (#const AF_INET6)
#else
    throw (SocketFamilyNotAvailable "AF_INET6")
#endif

socketFamilyUnix :: SocketFamily
socketFamilyUnix =
#ifdef AF_UNIX
    SocketFamily (#const AF_UNIX)
#else
    throw (SocketFamilyNotAvailable "AF_UNIX")
#endif

packFamily :: SocketFamily -> CInt
packFamily (SocketFamily sf) = sf

unpackFamily :: CInt -> SocketFamily
unpackFamily sf = SocketFamily sf

-- | This function returns the expected size of the SockAddr structure
-- for the given SocketFamily
sockAddrSize :: Integral int => SocketFamily -> int
sockAddrSize sf
#ifdef AF_INET
    | sf == socketFamilyInet  = #const sizeof(struct sockaddr_in)
#endif
#ifdef AF_INET6
    | sf == socketFamilyInet6 = #const sizeof(struct sockaddr_in6)
#endif
#ifdef AF_UNIX
    | sf == socketFamilyUnix  = #const sizeof(struct sockaddr_un)
#endif
    | otherwise = throw (SocketFamilyNotAvailable $ show sf)

newtype SocketMsgFlags = SocketMsgFlags CInt
    deriving (Storable)

instance Monoid SocketMsgFlags where
    mempty = SocketMsgFlags 0
    mappend (SocketMsgFlags f1) (SocketMsgFlags f2) =
        SocketMsgFlags (f1 .|. f2)

newtype CSockLen = CSockLen CInt
    deriving (Storable)

data MsgHdr = MsgHdr
    { msgHdrName     :: !(Ptr Word8)
    , msgHdrNameLen  :: !CInt
    , msgHdrIovecs   :: !IOVecs
    , msgHdrIovecLen :: !CInt
    , msgHdrFlags    :: !SocketMsgFlags
    }

type IOVecs = Ptr IOVec

data IOVec = IOVec
    { iovecData :: !(Ptr Word8)
    , iovecSize :: !CSize
    }

instance Storable MsgHdr where
    sizeOf _        = (#const sizeof(struct msghdr))
    alignment _     = 8
    peek ptr        =
        MsgHdr <$> (#peek struct msghdr, msg_name) ptr
               <*> (#peek struct msghdr, msg_namelen) ptr
               <*> (#peek struct msghdr, msg_iov) ptr
               <*> (#peek struct msghdr, msg_iovlen) ptr
               <*> (#peek struct msghdr, msg_flags) ptr
    poke ptr msghdr = do
        (#poke struct msghdr, msg_name)    ptr (msgHdrName msghdr)
        (#poke struct msghdr, msg_namelen) ptr (msgHdrNameLen msghdr)
        (#poke struct msghdr, msg_iov)     ptr (msgHdrIovecs msghdr)
        (#poke struct msghdr, msg_iovlen)  ptr (msgHdrIovecLen msghdr)
        (#poke struct msghdr, msg_flags)   ptr (msgHdrFlags msghdr)

instance Storable IOVec where
    sizeOf _        = (#const sizeof(struct iovec))
    alignment _     = 8
    peek ptr        =
        IOVec <$> (#peek struct iovec, iov_base) ptr
              <*> (#peek struct iovec, iov_len) ptr
    poke ptr iovec  = do
        (#poke struct iovec, iov_base) ptr (iovecData iovec)
        (#poke struct iovec, iov_len) ptr (iovecSize iovec)
