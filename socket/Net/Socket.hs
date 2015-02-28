-- |
-- Module      : Net.Socket
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Net.Socket
    ( SockAddr
    , SockAddrInet(..)
    , SockAddrInet6(..)
    , SockAddrUNIX(..)
    , connect
    , send
    , receive
    , listen
    , accept
    , marshalAddr
    , unMarshalAddr
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import Foreign.ForeignPtr
import Foreign.Ptr
import Net.Types
import Net.Socket.Address
import Net.Socket.System
import System.IO.Unsafe

data SockAddrInet = SockAddrInet IPv4Addr PortNumber
    deriving (Show, Eq)

-- as defined in sys/socket.h
-- sockaddr_in looks like:
-- * 2 bytes for the family
-- * 2 bytes for the port number
-- * 4 bytes for the IPv4 addr
-- * 8 bytes not used (keep blank)
instance SockAddr SockAddrInet where
    sockAddrToData (SockAddrInet addr port) = do
        putFamily AF_INET
        putN16 $ fromIntegral port
        putIPv4 addr
        replicateM_ 8 (put8 0)
    sockAddrFromData = do
        withFamily AF_INET
        port <- portnumber . fromIntegral <$> getN16
        addr <- getIPv4
        return $ SockAddrInet addr port
    sockAddrToParams _ = AF_INET

data SockAddrInet6 = SockAddrInet6 IPv6Addr PortNumber
    deriving (Show, Eq)

instance SockAddr SockAddrInet6 where
    sockAddrToData (SockAddrInet6 addr port) = do
        putFamily AF_INET6
        putN16 $ fromIntegral port
        putN32 0 -- TODO: flow label...
        putIPv6 addr
        putN32 0 -- TODO: scope ID
    sockAddrFromData = do
        withFamily AF_INET6
        port <- portnumber . fromIntegral <$> getN16
        label <- getN32
        addr <- getIPv6
        scopeid <- getN32
        unless (label == 0) $ sockAddrReaderError "expecting label == 0"
        unless (scopeid == 0) $ sockAddrReaderError "expecting scopeid == 0"
        return $ SockAddrInet6 addr port
    sockAddrToParams _ = AF_INET6

-- | for AF_UNIX (also known as AF_LOCAL)
data SockAddrUNIX = SockAddrUNIX String
    deriving (Show, Eq)

unixlen :: Integral int => int
unixlen = fromIntegral (108 :: Int)

-- as defined in sys/un.h
-- sockaddr_in looks like:
-- * 2 bytes for the family
-- * up to 107 bytes of FilePath
instance SockAddr SockAddrUNIX where
    sockAddrToData (SockAddrUNIX path) = do
        unless (length path < unixlen) $ sockAddrWriterError "path length too long"
        putFamily AF_UNIX
        mapM_ put8 $ map B.c2w path
        replicateM_ (unixlen - length path) (put8 0)
    sockAddrFromData = do
        withFamily AF_UNIX
        wl <- replicateM unixlen get8
        let (path, _) = span (> 0) wl
        return $ SockAddrUNIX $ map B.w2c path
    sockAddrToParams _ = AF_UNIX

withFamily :: SocketFamily -> SockAddrReader ()
withFamily sf = do
    family <- getFamily
    if family == sf
        then return ()
        else sockAddrReaderError $ "wrong family: expecting \"" ++ show sf ++ "\" by got: " ++ show family

-- | Create a connected socket
connect :: SockAddr addr
        => addr
        -> SocketType
        -> IO Socket
connect addr socketTy = do
    sock <- socketCreate (sockAddrToParams addr) socketTy 0
    socketConnect sock (marshalAddr addr)
    return sock

send :: Socket -> ByteString -> IO Int
send socket bs = socketSend socket bs socketMsgNormal

receive :: Socket -> Int -> IO ByteString
receive socket maxSize =
    socketRecv socket maxSize socketMsgNormal

-- | Create a listening socket
listen :: SockAddr addr
       => addr
       -> SocketType
       -> Int
       -> IO Socket
listen addr socketTy backlog = do
    sock <- socketCreate (sockAddrToParams addr) socketTy 0
    socketBind sock (marshalAddr addr)
    socketListen sock backlog
    return sock

accept :: Socket -> IO (Socket, SocketAddrRaw)
accept socket = socketAccept socket 14

marshalAddr :: SockAddr addr
            => addr
            -> SocketAddrRaw
marshalAddr addr = unsafePerformIO $ do
    bs <- B.createAndTrim maxMarshalAddrSize $ \ptr -> do
            (_, (_, l)) <- runSockAddrWriter (sockAddrToData addr) (ptr, maxMarshalAddrSize)
            return $ maxMarshalAddrSize - fromIntegral l
    return $ SocketAddrRaw bs

unMarshalAddr :: SockAddr addr
              => SocketAddrRaw
              -> addr
unMarshalAddr (SocketAddrRaw bs) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    (v, _) <- runSockAddrReader sockAddrFromData (ptr `plusPtr` off, fromIntegral len)
    return v
  where
    (fptr, off, len) = B.toForeignPtr bs

maxMarshalAddrSize :: Integral int => int
maxMarshalAddrSize = fromIntegral (256 :: Int)
