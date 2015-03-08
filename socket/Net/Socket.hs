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
    ( -- * SockAddr
      SockAddr(..)
    , marshalAddr
    , unMarshalAddr
      -- ** Implemented types
    , SockAddrInet4(..)
    , SockAddrInet6(..)
    , SockAddrUNIX(..)
      -- * Socket
    , Socket
    , SocketType(..)
      -- ** Connections
    , connect
      -- ** Accepting connections
    , bind
    , listen
    , accept
      -- ** send/receive
    , send
    , receive
      -- ** Close/shutdown
    , close
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Net.Types
import Net.Socket.Address
import Net.Socket.System
import System.IO.Unsafe

-- little helper function to to validate the SocketFamily
-- while unmarshaling a SockAddr
unlessFamily :: SocketFamily -> SockAddrReader ()
unlessFamily sf = do
    family <- getFamily
    if family == sf
        then return ()
        else sockAddrReaderError $ "wrong family: expecting \"" ++ show sf ++ "\" but got: " ++ show family

unlessSize :: Word8 -> SockAddrReader ()
unlessSize size = do
    r <- get8
    if r == size
        then return ()
        else sockAddrReaderError $ "wrong size: expecting " ++ show size ++ " but got: " ++ show r

-------------------------------------------------------------------------------
--                          SockAddr Types                                   --
-------------------------------------------------------------------------------

-- | Create a SockAddr for IPv4
data SockAddrInet4 = SockAddrInet4 IPv4Addr PortNumber
    deriving (Show, Eq)

-- as defined in sys/socket.h
-- sockaddr_in looks like:
-- * 1 byte for size
-- * 1 byte for the family
-- * 2 bytes for the port number
-- * 4 bytes for the IPv4 addr
-- * 8 bytes not used (keep blank)
instance SockAddr SockAddrInet4 where
    sockAddrToData (SockAddrInet4 addr port) = do
        put8 $ sockAddrSize socketFamilyInet4
        putFamily socketFamilyInet4
        putN16 $ fromIntegral port
        putIPv4 addr
        replicateM_ 8 (put8 0)
    sockAddrFromData = do
        unlessSize $ sockAddrSize socketFamilyInet4
        unlessFamily socketFamilyInet4
        port <- portnumber . fromIntegral <$> getN16
        addr <- getIPv4
        return $ SockAddrInet4 addr port
    sockAddrToParams _ = socketFamilyInet4

-- | Create a SockAddr for IPv6
data SockAddrInet6 = SockAddrInet6 IPv6Addr PortNumber
    deriving (Show, Eq)

instance SockAddr SockAddrInet6 where
    sockAddrToData (SockAddrInet6 addr port) = do
        put8 $ sockAddrSize socketFamilyInet6
        putFamily socketFamilyInet6
        putN16 $ fromIntegral port
        putN32 0 -- TODO: flow label...
        putIPv6 addr
        putN32 0 -- TODO: scope ID
    sockAddrFromData = do
        unlessSize $ sockAddrSize socketFamilyInet6
        unlessFamily socketFamilyInet6
        port <- portnumber . fromIntegral <$> getN16
        label <- getN32
        addr <- getIPv6
        scopeid <- getN32
        unless (label == 0) $ sockAddrReaderError "expecting label == 0"
        unless (scopeid == 0) $ sockAddrReaderError "expecting scopeid == 0"
        return $ SockAddrInet6 addr port
    sockAddrToParams _ = socketFamilyInet6

-- | SockAddr for UNIX (or LOCAL)
data SockAddrUNIX = SockAddrUNIX String
    deriving (Show, Eq)

-- as specified for Unix SockAddr
-- the max size of the FilePath is n bytes long
-- (also need to consider that it is expected to terminate the String with
-- an empty byte -- \x00)
unixlen :: Integral int => int
unixlen = fromIntegral $ (sockAddrSize socketFamilyUnix :: Int) - 2

-- as defined in sys/un.h
-- sockaddr_in looks like:
-- * 1 byte for size
-- * 1 byte for the family
-- * up to n bytes of FilePath (108 on linux, 104 on OSX...)
instance SockAddr SockAddrUNIX where
    sockAddrToData (SockAddrUNIX path) = do
        put8 $ sockAddrSize socketFamilyUnix
        unless (length path < unixlen) $ sockAddrWriterError "path length too long"
        putFamily socketFamilyUnix
        mapM_ put8 $ map B.c2w path
        replicateM_ (unixlen - length path) (put8 0)
    sockAddrFromData = do
        unlessSize $ sockAddrSize socketFamilyUnix
        unlessFamily socketFamilyUnix
        wl <- replicateM unixlen get8
        let (path, _) = span (> 0) wl
        return $ SockAddrUNIX $ map B.w2c path
    sockAddrToParams _ = socketFamilyUnix

-------------------------------------------------------------------------------
--                          SockAddr to SockAddrRaw                          --
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
--                          Action on Socket                                 --
-------------------------------------------------------------------------------

-- | Create a socket and connect it to the given SockAddr
connect :: SockAddr addr
        => addr
        -> SocketType
        -> IO Socket
connect addr socketTy = do
    sock <- socketCreate (sockAddrToParams addr) socketTy 0
    socketConnect sock (marshalAddr addr)
    return sock

bind :: SockAddr addr
     => addr
     -> SocketType
     -> IO Socket
bind addr socketTy = do
    sock <- socketCreate (sockAddrToParams addr) socketTy 0
    socketBind sock (marshalAddr addr)
    return sock

-- | Create a socket and bind it to the given SockAddr
-- and also listen on this created socket
listen :: Socket
       -> Int
       -> IO Socket
listen sock backlog = do
    socketListen sock backlog
    return sock

-- | Accept connection from the given Socket
accept :: SockAddr a
       => Socket
       -> IO (Socket, a)
accept socket = do
    (sClient, saClient) <- socketAccept socket 256
    return (sClient, unMarshalAddr saClient)

close :: Socket -> IO ()
close = socketClose

-- | send a ByteString to the given socket
-- the returned integer is the size of the sent data
send :: Socket -> ByteString -> IO Int
send socket bs = socketSend socket bs socketMsgNormal

-- | receive a ByteString from the given socket
--
-- In case of a TCP connection:
-- If the returned bytestring is empty, that means the peer has closed
-- its half side of connection.
receive :: Socket
        -> Int -- ^ the maximum size to accept from this socket
        -> IO ByteString -- ^ the length of the returned bytestring is less or equal the given max size.
receive socket maxSize =
    socketRecv socket maxSize socketMsgNormal
