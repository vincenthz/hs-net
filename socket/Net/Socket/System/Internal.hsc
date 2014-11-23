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
    ) where

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

import Control.Applicative
import Data.Bits
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr

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
