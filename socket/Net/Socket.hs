-- |
-- Module      : Data.Git
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Net.Socket
    (
    ) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr (Ptr)

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

-- FIXME types
data CSockAddr
data CMsgHdr
newtype CSockLen = CSockLen CInt
