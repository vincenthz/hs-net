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
    , connect
    , listen
    ) where

import Data.Word
import Net.Types
import Net.Socket.Address
import Net.Socket.System

newtype Port = Port Word16
    deriving (Show,Eq,Ord)

data InetAddr = IPv4 IPv4Addr | IPv6 IPv6Addr
data SockAddrInet = SockAddrInet InetAddr Port

instance SockAddr SockAddrInet where
    sockAddrToData   = undefined
    sockAddrFromData = undefined
    sockAddrToParams = undefined
{- len family port addr (0 [8]) -}

{-
instance SockAddr UnixAddr where
    sockAddrToData   = undefined
    sockAddrFromData = undefined
    sockAddrToParams = undefined
-}

-- | Create a connected socket
connect :: SockAddr addr
        => addr
        -> SocketType
        -> IO Socket
connect addr socketTy = do
    sock <- socketCreate (sockAddrToParams addr) socketTy 0
    socketConnect sock (marshalAddr addr)
    return sock

{-
connect TCP (SockAddrInet (ipv4 (10,20,30,40)) 80)
connect TCP (Unix "/")
-}

{-
connect (TCP $ ipv4 (10,20,30,40))
connect (UDP $ ipv6 (0x2901,0x0,0x1,0x2,0x3,0x4,0x5,0x6))
connect (Unix "/unix/path")
-}

-- | Create a listening socket
listen :: SockAddr addr => addr -> SocketType -> Int -> IO Socket
listen addr socketTy backlog = do
    sock <- socketCreate (sockAddrToParams addr) socketTy 0
    socketBind sock (marshalAddr addr)
    socketListen sock backlog
    return sock

marshalAddr :: SockAddr addr => addr -> SocketAddrRaw
marshalAddr = undefined
