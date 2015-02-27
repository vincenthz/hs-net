module Main (main) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Net.Socket
import Net.Socket.System

import System.Environment

startServerOn :: SockAddr addr => addr -> IO ()
startServerOn addr = do
    s <- connect addr Stream
    forever $ do
        bs <- BC.pack <$> getLine
        sz <- send s bs
        if sz /= BC.length bs
            then error $ "didn't send all the data: " ++ show sz
            else return ()

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
