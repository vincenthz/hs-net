-- |
-- Module      : Net.Socket.System
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Net.Socket.System
    ( socketCreate
    , socketConnect
    , socketBind
    , socketListen
    , socketAccept
    , socketRecv
    , socketRecvFrom
    , socketSend
    , socketSendTo
    , SocketType
    , socketTypeStream
    , socketTypeDatagram
    , socketTypeRaw
    -- * Raw socket type
    , Socket
    , SocketAddrRaw(..)
    , SocketMsgFlags(..)
    , socketMsgNormal
    , socketMsgOOB
    , socketMsgPeek
    , socketMsgDontRoute
    , socketMsgWaitAll
    ) where

import Control.Exception
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import Data.Monoid
import Data.Word
import Data.Typeable
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc
import Net.Socket.Address

-- exceptions

data SocketError =
      SocketError_ConnectionRefused
    | SocketError_ConnectionReset
    | SocketError_AddressInUse
    | SocketError_AddressNotAvailable
    | SocketError_AddressCannotBeUseWithSocketType
    | SocketError_InvalidDescriptor
    | SocketError_NetworkFailure
    | SocketError_System CInt
    deriving (Show,Eq,Typeable)

instance Exception SocketError

newtype SocketType = SocketType Int -- FIXME, AF_INET6, ?

newtype Socket = Socket CInt

newtype SocketAddrRaw = SocketAddrRaw ByteString

socketTypeStream :: SocketType
socketTypeStream = SocketType 1

socketTypeDatagram :: SocketType
socketTypeDatagram = SocketType 2

socketTypeRaw :: SocketType
socketTypeRaw = SocketType 3

-- | create an unconnected socket
socketCreate :: SocketFamily
             -> SocketType
             -> Int
             -> IO Socket
socketCreate (SocketFamily domain) (SocketType ty) protocol = do
    onError "socketCreate" Socket =<< c_socket (fromIntegral domain) (fromIntegral ty) (fromIntegral protocol)

-- | Initiate a connection to an adress on a socket
socketConnect :: Socket
              -> SocketAddrRaw
              -> IO ()
socketConnect (Socket socket) addrRaw =
    onError "socketConnect" (const ()) =<< withSocketAddrRaw addrRaw (\ptr len -> c_connect socket ptr len)

-- | Bind a name to a socket
socketBind :: Socket -> SocketAddrRaw -> IO ()
socketBind (Socket socket) addrRaw = do
    onError "socketBind" (const ()) =<< withSocketAddrRaw addrRaw (\ptr len -> c_bind socket ptr len)

-- | Make the system listen for connection on a socket
socketListen :: Socket -> Int -> IO ()
socketListen (Socket socket) backlog = do
    onError "socketListen" (const ()) =<< c_listen socket (fromIntegral backlog)

-- | Accept a connection on a listening socket
--
-- On success the new socket is returned along with the address of the connecting entity
socketAccept :: Socket -> Int -> IO (Socket, SocketAddrRaw)
socketAccept (Socket socket) sAddrSz = do
    sAddr <- B.mallocByteString sAddrSz
    alloca $ \sAddrLenPtr -> do
        poke sAddrLenPtr (CSockLen $ fromIntegral sAddrSz)
        accepted <- withForeignPtr sAddr $ \sAddrPtr ->
            onError "socketAccept" Socket =<< c_accept socket (castPtr sAddrPtr) sAddrLenPtr
        (CSockLen sockLen) <- peek sAddrLenPtr
        return (accepted, SocketAddrRaw $! B.PS sAddr 0 (fromIntegral sockLen))

data SocketMsgFlags = SocketMsgFlags CInt

instance Monoid SocketMsgFlags where
    mempty = SocketMsgFlags 0
    mappend (SocketMsgFlags f1) (SocketMsgFlags f2) =
        SocketMsgFlags (f1 .|. f2)

socketMsgNormal :: SocketMsgFlags
socketMsgNormal = mempty

-- could be bogus values related to certain system.
socketMsgOOB :: SocketMsgFlags
socketMsgOOB = SocketMsgFlags 0x1

socketMsgPeek :: SocketMsgFlags
socketMsgPeek = SocketMsgFlags 0x2

socketMsgDontRoute :: SocketMsgFlags
socketMsgDontRoute = SocketMsgFlags 0x4

socketMsgWaitAll :: SocketMsgFlags
socketMsgWaitAll = SocketMsgFlags 0x40

-- | Try to get up to @len size data from a socket.
--
-- The call returned as soon as some data is available,
-- the bytestring is allocated to be of maximum size, but
-- is trimmed on partial filled (however the memory is not re-allocated).
socketRecv :: Socket -> Int -> SocketMsgFlags -> IO ByteString
socketRecv (Socket socket) len (SocketMsgFlags flags) =
    B.createAndTrim len $ \ptr -> do
        ret <- c_recv socket ptr (fromIntegral len) flags
        onErrorSz "socketRecv" fromIntegral $ ret

-- | Similar to 'socketRecv' but also returns the socket address associated with the data.
socketRecvFrom :: Socket -> Int -> SocketMsgFlags -> IO (ByteString, SocketAddrRaw)
socketRecvFrom (Socket socket) len (SocketMsgFlags flags) = do
    let sAddrSz = 256 -- FIXME
    sAddr <- B.mallocByteString sAddrSz
    alloca $ \sAddrLenPtr -> do
        poke sAddrLenPtr (CSockLen $ fromIntegral sAddrSz)
        bs <- B.createAndTrim len $ \dataPtr ->
            withForeignPtr sAddr $ \sAddrPtr -> do
                ret <- c_recvfrom socket dataPtr (fromIntegral len) flags (castPtr sAddrPtr) (castPtr sAddrLenPtr)
                onErrorSz "socketRecvFrom" fromIntegral $ ret
        (CSockLen sockLen) <- peek sAddrLenPtr
        return (bs, SocketAddrRaw $! B.PS sAddr 0 (fromIntegral sockLen))

-- | Try to send some data into a socket
--
-- It returns the number of bytes consumed.
socketSend :: Socket -> ByteString -> SocketMsgFlags -> IO Int
socketSend (Socket socket) bs (SocketMsgFlags flags) =
    withForeignPtr fptr $ \ptr -> do
        ret <- c_send socket (ptr `plusPtr` ofs) (fromIntegral len) flags
        onErrorSz "socketSend" fromIntegral $ ret
  where (fptr, ofs, len) = B.toForeignPtr bs

-- | Similar to 'socketSend' but instead of using the socket connected destination,
-- it used an explicitely given socket address.
socketSendTo :: Socket -> ByteString -> SocketMsgFlags -> SocketAddrRaw -> IO Int
socketSendTo (Socket socket) dat (SocketMsgFlags flags) addrRaw =
    withSocketAddrRaw addrRaw $ \sAddrPtr sAddrLen ->
    withForeignPtr sData $ \dataPtr -> do
        ret <- c_sendto socket (dataPtr `plusPtr` dataOfs) (fromIntegral dataLen) flags sAddrPtr sAddrLen
        onErrorSz "socketRecvFrom" fromIntegral $ ret
  where (sData, dataOfs, dataLen) = B.toForeignPtr dat

throwSocketErrno :: String -> IO a
throwSocketErrno fctName = do
    errno <- getErrno
    let err = errnoToSocketError errno
    error ("this is a temporary error, it should raise something linked to errno or better a specific exception: " ++ fctName ++ " : " ++ show err)
  where errnoToSocketError errno@(Errno errnoVal)
            | errno == eADDRINUSE      = SocketError_AddressInUse
            | errno == eADDRNOTAVAIL   = SocketError_AddressNotAvailable
            | errno == eAFNOSUPPORT    = SocketError_AddressCannotBeUseWithSocketType
            | errno == eBADF           = SocketError_InvalidDescriptor
            | errno == eCONNREFUSED    = SocketError_ConnectionRefused
            | errno == eCONNRESET      = SocketError_ConnectionReset
            | errno == eNETDOWN        = SocketError_NetworkFailure
            | errno == eNETRESET       = SocketError_NetworkFailure
            | errno == eNETUNREACH     = SocketError_NetworkFailure
            | errno == eNOTSOCK        = SocketError_InvalidDescriptor
            | otherwise                = SocketError_System errnoVal

onError :: String -> (CInt -> a) -> CInt -> IO a
onError fctName mapper v
    | v == -1   = throwSocketErrno fctName
    | otherwise = return $ mapper v

onErrorSz :: String -> (CSize -> a) -> CSize -> IO a
onErrorSz fctName mapper v
    | v == -1   = throwSocketErrno fctName
    | otherwise = return $ mapper v

withSocketAddrRaw :: SocketAddrRaw -> (Ptr CSockAddr -> CSockLen -> IO a) -> IO a
withSocketAddrRaw (SocketAddrRaw bs) f =
    withForeignPtr fptr $ \ptr -> f (castPtr (ptr `plusPtr` ofs)) (CSockLen $ fromIntegral len)
  where (fptr, ofs, len) = B.toForeignPtr bs

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
{-
foreign import ccall unsafe "sendmsg"
    c_sendmsg :: CInt -> Ptr CMsgHdr -> CInt -> IO CSize
-}
foreign import ccall unsafe "sendto"
    c_sendto :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr CSockAddr -> CSockLen -> IO CSize
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSize
{-
foreign import ccall unsafe "recvmsg"
    c_recvmsg :: CInt -> Ptr CMsgHdr -> CInt -> IO CSize
-}
foreign import ccall unsafe "recvfrom"
    c_recvfrom :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr CSockAddr -> Ptr CSockLen -> IO CSize
{-
foreign import ccall unsafe "getpeername"
    c_getpeername :: CInt -> Ptr CSockAddr -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "getsockname"
    c_getsockname :: CInt -> Ptr CSockAddr -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "getsockopt"
    c_getsockopt :: CInt -> CInt -> CInt -> Ptr () -> Ptr CSockLen -> IO CInt
foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr () -> CSockLen -> IO CInt
-}

-- FIXME types
data CSockAddr
--data CMsgHdr

newtype CSockLen = CSockLen CInt
    deriving (Storable)
