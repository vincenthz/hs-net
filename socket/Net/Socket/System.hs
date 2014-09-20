-- |
-- Module      : Net.Socket.System
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE ForeignFunctionInterface #-}
module Net.Socket.System
    ( socketCreate
    , socketConnect
    , socketBind
    , socketListen
    , socketAccept
    , SocketType
    , Socket
    ) where

import Control.Applicative
import Data.Word
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Net.Socket.Address

newtype SocketType = SocketType Int -- FIXME, AF_INET6, 
data Socket = Socket Int

-- | create an unconnected socket
socketCreate :: SocketFamily
             -> SocketType
             -> IO Socket
socketCreate (SocketFamily domain) (SocketType ty) protocol = do
    c_socket domain ty protocol

socketConnect :: SockAddr sockaddr
              => Socket
              -> sockAddr
              -> IO ()
socketConnect socket sockaddr =
    undefined

socketBind = undefined
socketListen = undefined

socketAccept = undefined

-- C socket api
-- > int socket(int domain, int type, int protocol);
-- > int bind(int socket, const struct sockaddr *address, socklen_t address_len);
-- > int listen(int socket, int backlog);
-- > int accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
-- > int connect(int socket, const struct sockaddr *address, socklen_t address_len);
--
-- > ssize_t send(int socket, const void *buffer, size_t length, int flags);
-- > ssize_t sendmsg(int socket, const struct msghdr *message, int flags);
-- > ssize_t sendto(int socket, const void *buffer, size_t length, int flags, const struct sockaddr *dest_addr, socklen_t dest_len);
--
-- > ssize_t recv(int socket, void *buffer, size_t length, int flags);
-- > ssize_t recvfrom(int socket, void *restrict buffer, size_t length, int flags, struct sockaddr *restrict address, socklen_t *restrict address_len);
-- > ssize_t recvmsg(int socket, struct msghdr *message, int flags);
--
-- > int getpeername(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
-- > int getsockname(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
-- > int getsockopt(int socket, int level, int option_name, void *restrict option_value, socklen_t *restrict option_len);
-- > int setsockopt(int socket, int level, int option_name, const void *option_value, socklen_t option_len);
--

foreign import ccall unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "bind"
    c_bind :: CInt -> Ptr CSockAddr -> CSockLen -> IO CInt
foreign import ccall unsafe "listen"
    c_listen :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "accept"
    c_accept :: CInt -> Ptr CSockAddr -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "connect"
    c_connect :: CInt -> Ptr CSockAddr -> CSockLen -> IO CInt
foreign import ccall unsafe "send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSize
foreign import ccall unsafe "sendmsg"
    c_sendmsg :: CInt -> Ptr CMsgHdr -> CInt -> IO CSize
foreign import ccall unsafe "sendto"
    c_sendto :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr CSockAddr -> CSockLen -> IO CSize
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSize
foreign import ccall unsafe "recvmsg"
    c_recvmsg :: CInt -> Ptr CMsgHdr -> CInt -> IO CSize
foreign import ccall unsafe "recvfrom"
    c_recvfrom :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr CSockAddr -> CSockLen -> IO CSize
foreign import ccall unsafe "getpeername"
    c_getpeername :: CInt -> Ptr CSockAddr -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "getsockname"
    c_getsockname :: CInt -> Ptr CSockAddr -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "getsockopt"
    c_getsockopt :: CInt -> CInt -> CInt -> Ptr () -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr () -> CSockLen -> IO CInt

-- FIXME types
data CSockAddr
data CMsgHdr
newtype CSockLen = CSockLen CInt
