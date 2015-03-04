-- |
-- Module      : Net.Socket.Address
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--

{-# LANGUAGE OverloadedStrings #-}

module Scenario
    ( ScenarioConfig(..)
    , defaultScenarioConfig
    , startTest
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import Net.Socket

data ScenarioConfig addr = ScenarioConfig
    { scSocketType      :: SocketType

    -- Server options --------
    , scNumAcceptedConn :: Int -- ^ the listen backlog
    , scMaxSizeReceive  :: Int
    , scSockAddrServer  :: addr

    -- Client options --------
    , scSockAddrClient  :: addr
    } deriving (Show, Eq)

defaultScenarioConfig :: SockAddr addr => addr -> addr -> SocketType -> ScenarioConfig addr
defaultScenarioConfig addrServer addrClient sockType = ScenarioConfig
    { scSocketType      = sockType

    -- Server options --------
    , scNumAcceptedConn = 10
    , scMaxSizeReceive  = 256
    , scSockAddrServer  = addrServer

    -- Client options --------
    , scSockAddrClient  = addrClient
    }

startTest :: (Show addr, SockAddr addr)
          => ScenarioConfig addr
          -> IO (Either String String)
startTest scfg = do
    chan <- newScenarioTChan
    serverThreadId <- forkIO $ server scfg chan
    ref <- client scfg
    (info, bs) <- popScenarioTChan chan
    killThread serverThreadId
    return $ if ref /= bs
        then Left $ "the data are different: " ++ show ref ++ " /= " ++ show bs
        else Right $ show info ++ " sent " ++ show bs ++ " to " ++ show (scSockAddrServer scfg)

client :: SockAddr addr
       => ScenarioConfig addr
       -> IO ByteString
client scfg = do
    s <- connect (scSockAddrClient scfg) (scSocketType scfg)
    size <- send s plain
    close s
    return $ case size == BC.length plain of
        True                           -> plain
        False | size == 0              -> BC.empty
        False | size < 0               -> BC.empty
        False | size < BC.length plain -> BC.take size plain
        False | otherwise              -> BC.empty
  where
    plain :: ByteString
    plain = "this is a random string to send to the server"

server :: SockAddr addr
       => ScenarioConfig addr
       -> ScenarioTChan addr
       -> IO ()
server scfg chan = do
    s <- listen (scSockAddrServer scfg) (scSocketType scfg) (scNumAcceptedConn scfg)
    forever $ do
        (socket, info) <- accept s
        forkIO $ serverLoop scfg chan socket info

serverLoop :: SockAddr addr
           => ScenarioConfig addr
           -> ScenarioTChan addr
           -> Socket
           -> addr
           -> IO ()
serverLoop scfg chan s info = do
    bs <- receive s (scMaxSizeReceive scfg)
    putScenarioTChan chan (info, bs)
    if BC.length bs == 0
        then close s
        else serverLoop scfg chan s info

----

type ScenarioTChan addr = TChan (addr, ByteString)

newScenarioTChan :: SockAddr addr => IO (ScenarioTChan addr)
newScenarioTChan = atomically $ newTChan

putScenarioTChan :: SockAddr addr => (ScenarioTChan addr) -> (addr, ByteString) -> IO ()
putScenarioTChan chan val = atomically $ writeTChan chan val

popScenarioTChan :: SockAddr addr => (ScenarioTChan addr) -> IO (addr, ByteString)
popScenarioTChan = atomically . readTChan
