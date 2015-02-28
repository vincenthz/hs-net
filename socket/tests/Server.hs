module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as BC
import Net.Socket
import Net.Socket.System

import System.Environment

startServerOn :: SockAddr addr => addr -> IO ()
startServerOn addr = do
    s <- listen addr Stream 12
    forever $ do
        (client, clientInfo) <- accept s
        putStrLn $ "accpecting connection from: " ++ show clientInfo
        forkIO $ forever $ do
            str <- BC.unpack <$> receive client 512
            putStrLn $ "receive data: " ++ show str

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        "unix":path:[]      -> startServerOn $ SockAddrUNIX path
        "ipv4":addr:port:[] -> startServerOn $ SockAddrInet  (read addr) (read port)
        "ipv6":addr:port:[] -> startServerOn $ SockAddrInet6 (read addr) (read port)
        _ -> usage

usage :: IO ()
usage = do
    putStr "  client unix <filepath>"
    putStr "  client ipv4 <address> <port>"
    putStr "  client ipv6 <address> <port>"
