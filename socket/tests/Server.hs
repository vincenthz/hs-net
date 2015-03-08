module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as BC
import Net.Socket
import Net.Socket.System

import System.Environment

debug :: String -> IO ()
debug = putStrLn

startServerTCP :: (Show addr, SockAddr addr) => addr -> IO ()
startServerTCP addr = do
    s <- bind addr Stream
    s <- listen s 12
    forever $ do
        (client, clientInfo) <- acceptFunction addr s
        debug $ "accpecting connection from: " ++ show clientInfo
        forkIO $ connectionLoop client
  where
    acceptFunction :: (Show addr, SockAddr addr)
                   => addr
                   -> Socket
                   -> IO (Socket, addr)
    acceptFunction _ s = accept s

startServerUDP :: (Show addr, SockAddr addr) => addr -> IO ()
startServerUDP addr = do
    s <- bind addr Datagram
    forever $ connectionLoop s

startServerOn :: (Show addr, SockAddr addr)
              => String
              -> addr
              -> IO ()
startServerOn t addr =
    case t of
        "tcp" -> startServerTCP addr
        "udp" -> startServerUDP addr
        _     -> error $ "Socket Family Type not found: " ++ show t

connectionLoop :: Socket -> IO ()
connectionLoop s = do
    str <- BC.unpack <$> receive s 512
    if length str == 0
        then do
            debug $ "client closed connection"
            close s
        else do
            debug $ "receive data: " ++ show str
            connectionLoop s

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        "unix":t:path:[]      -> startServerOn t $ SockAddrUNIX path
        "ipv4":t:addr:port:[] -> startServerOn t $ SockAddrInet4 (read addr) (read port)
        "ipv6":t:addr:port:[] -> startServerOn t $ SockAddrInet6 (read addr) (read port)
        _ -> usage

usage :: IO ()
usage = do
    putStrLn "  client unix <filepath>"
    putStrLn "  client ipv4 <address> <port>"
    putStrLn "  client ipv6 <address> <port>"
