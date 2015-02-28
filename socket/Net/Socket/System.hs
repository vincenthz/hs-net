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
{-# LANGUAGE BangPatterns #-}
module Net.Socket.System
    ( socketCreate
    , socketConnect
    , socketBind
    , socketListen
    , socketAccept
    , socketShutdown
    , socketRecv
    , socketRecvFrom
    , socketSend
    , socketSendTo
    , socketSendVec
    , SocketType(..)
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import Data.Monoid
import Data.Word
import Data.Typeable
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Net.Socket.Address
import Net.Socket.System.Internal

import GHC.Conc (threadWaitRead, threadWaitWrite)
import System.Posix.Types
import System.Posix.Internals (setNonBlockingFD)

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

newtype Socket = Socket CInt

-- | create an unconnected socket
socketCreate :: SocketFamily
             -> SocketType
             -> Int
             -> IO Socket
socketCreate domain sockType protocol =
    checkRet "socketCreate" id (c_socket (fromIntegral $ packFamily domain) (fromIntegral $ packSocketType sockType) (fromIntegral protocol)) >>= setupSocket
  where setupSocket :: CInt -> IO Socket
        setupSocket socket = do
            setNonBlockingFD socket False
            return $ Socket socket

-- | Initiate a connection to an adress on a socket
socketConnect :: Socket
              -> SocketAddrRaw
              -> IO ()
socketConnect (Socket socket) addrRaw =
    withSocketAddrRaw addrRaw $ \ptr len ->
        checkRet "socketConnect" (const ()) (c_connect socket ptr len)

-- | Bind a name to a socket
socketBind :: Socket -> SocketAddrRaw -> IO ()
socketBind (Socket socket) addrRaw =
    withSocketAddrRaw addrRaw $ \ptr len -> do
        checkRet "socketBind" (const ()) (c_bind socket ptr len)

-- | Make the system listen for connection on a socket
socketListen :: Socket -> Int -> IO ()
socketListen (Socket socket) backlog =
    checkRet "socketListen" (const ()) $ c_listen socket (fromIntegral backlog)

-- | Accept a connection on a listening socket
--
-- On success the new socket is returned along with the address of the connecting entity
socketAccept :: Socket -> Int -> IO (Socket, SocketAddrRaw)
socketAccept (Socket socket) sAddrSz = do
    sAddr <- B.mallocByteString sAddrSz
    alloca $ \sAddrLenPtr -> do
        poke sAddrLenPtr (CSockLen $ fromIntegral sAddrSz)
        accepted <- withForeignPtr sAddr $ \sAddrPtr ->
            checkRet "socketAccept" Socket $ do
                threadWaitRead (Fd socket)
                c_accept socket (castPtr sAddrPtr) sAddrLenPtr
        (CSockLen sockLen) <- peek sAddrLenPtr
        return (accepted, SocketAddrRaw $! B.PS sAddr 0 (fromIntegral sockLen))

data ShutdownCommand = Shutdown_Read | Shutdown_Write | Shutdown_ReadWrite
    deriving (Show,Eq)

socketShutdown :: Socket -> ShutdownCommand -> IO ()
socketShutdown (Socket socket) command =
    checkRet "socketShutdown" (const ()) $ c_shutdown socket cVal
  where cVal = case command of
                   Shutdown_Read      -> 0
                   Shutdown_Write     -> 1
                   Shutdown_ReadWrite -> 2

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
        checkRet "socketRecv" fromIntegral $ do
            threadWaitRead (Fd socket)
            c_recv socket ptr (fromIntegral len) flags

-- | Similar to 'socketRecv' but also returns the socket address associated with the data.
socketRecvFrom :: Socket -> Int -> SocketMsgFlags -> IO (ByteString, SocketAddrRaw)
socketRecvFrom (Socket socket) len (SocketMsgFlags flags) = do
    let sAddrSz = 256 -- FIXME
    sAddr <- B.mallocByteString sAddrSz
    alloca $ \sAddrLenPtr -> do
        poke sAddrLenPtr (CSockLen $ fromIntegral sAddrSz)
        bs <- B.createAndTrim len $ \dataPtr ->
            withForeignPtr sAddr $ \sAddrPtr ->
                checkRet "socketRecvFrom" fromIntegral $ do
                    threadWaitRead (Fd socket)
                    c_recvfrom socket dataPtr (fromIntegral len) flags (castPtr sAddrPtr) (castPtr sAddrLenPtr)
        (CSockLen sockLen) <- peek sAddrLenPtr
        return (bs, SocketAddrRaw $! B.PS sAddr 0 (fromIntegral sockLen))

-- | Try to send some data into a socket
--
-- It returns the number of bytes consumed.
socketSend :: Socket -> ByteString -> SocketMsgFlags -> IO Int
socketSend (Socket socket) bs (SocketMsgFlags flags) =
    withForeignPtr fptr $ \ptr ->
        checkRet "socketSend" fromIntegral $ do
            threadWaitWrite (Fd socket)
            c_send socket (ptr `plusPtr` ofs) (fromIntegral len) flags
  where (fptr, ofs, len) = B.toForeignPtr bs

-- | Similar to 'socketSend' but instead of using the socket connected destination,
-- it used an explicitely given socket address.
socketSendTo :: Socket -> ByteString -> SocketMsgFlags -> SocketAddrRaw -> IO Int
socketSendTo (Socket socket) dat (SocketMsgFlags flags) addrRaw =
    withSocketAddrRaw addrRaw $ \sAddrPtr sAddrLen ->
    withForeignPtr sData $ \dataPtr ->
        checkRet "socketRecvFrom" fromIntegral $ do
            threadWaitRead (Fd socket)
            c_sendto socket (dataPtr `plusPtr` dataOfs) (fromIntegral dataLen) flags sAddrPtr sAddrLen
  where (sData, dataOfs, dataLen) = B.toForeignPtr dat

socketSendVec :: Socket -> [ByteString] -> SocketMsgFlags -> Maybe SocketAddrRaw -> IO Int
socketSendVec (Socket socket) dats flags addrRaw =
    withMsgHdr dats flags addrRaw $ \msgHdrPtr ->
        checkRet "socketSend" fromIntegral $ do
            threadWaitWrite (Fd socket)
            c_sendmsg socket msgHdrPtr 0


withMsgHdr :: [ByteString] -> SocketMsgFlags -> Maybe SocketAddrRaw -> (Ptr MsgHdr -> IO a) -> IO a
withMsgHdr dats flags addrRaw f =
    withOptionalBytes addrRaw $ \sAddrPtr sAddrLen ->
    allocaArray nbVecs $ \iovecs -> do
        mapM_ (toIOVec iovecs) $ zip [0..] dats
        alloca $ \msgHdrPtr -> do
            poke msgHdrPtr $ MsgHdr { msgHdrName     = sAddrPtr
                                    , msgHdrNameLen  = fromIntegral sAddrLen
                                    , msgHdrIovecs   = iovecs
                                    , msgHdrIovecLen = fromIntegral nbVecs
                                    , msgHdrFlags    = flags
                                    }
            f msgHdrPtr
  where
        !nbVecs = length dats
        toIOVec arrayPtr (i,bs) = do
            let (fptr, ofs, len) = B.toForeignPtr bs
            withForeignPtr fptr $ \ptr -> poke cellPtr $ IOVec (ptr `plusPtr` ofs) (fromIntegral len)
          where cellPtr = arrayPtr `plusPtr` (i * sizeOf (undefined :: IOVec))
        withOptionalBytes Nothing                                    fin = fin nullPtr 0
        withOptionalBytes (Just (SocketAddrRaw (B.PS fptr ofs len))) fin = withForeignPtr fptr $ \ptr -> fin (ptr `plusPtr` ofs) len

errnoToSocketError :: Errno -> SocketError
errnoToSocketError errno@(Errno errnoVal)
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

checkRet :: (Num ret, Eq ret) => String -> (ret -> a) -> IO ret -> IO a
checkRet _fctName mapper action = do
    r <- action
    if r == -1
        then do errno <- getErrno
                if errno == eAGAIN || errno == eWOULDBLOCK
                    then checkRet _fctName mapper action
                    else throwIO $ errnoToSocketError errno
        else return $ mapper r

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
-- > int shutdown(int socket, int how);
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
foreign import ccall unsafe "shutdown"
    c_shutdown :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSize
foreign import ccall unsafe "sendmsg"
    c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSize
foreign import ccall unsafe "sendto"
    c_sendto :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr CSockAddr -> CSockLen -> IO CSize
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSize
{-
foreign import ccall unsafe "recvmsg"
    c_recvmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSize
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
