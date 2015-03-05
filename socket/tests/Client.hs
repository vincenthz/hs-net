module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Net.Socket

import System.Environment
import System.Exit

startServerOn :: SockAddr addr => addr -> IO ()
startServerOn addr = do
    s <- connect addr Stream
    forever $ do
        str <- getLine
        putStrLn $ show str
        case span ((/=) ' ') str of
            ("close", _)    -> do
                close s
                exitSuccess
            ("send", ' ':v) -> do
                let bs = BC.pack v
                sz <- send s bs
                if sz /= BC.length bs
                    then error $ "didn't send all the data: " ++ show sz
                    else return ()
            v -> do
                print v
                putStrLn "Command Line Usage:"
                putStrLn "  close"
                putStrLn "  send <string to send>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        "unix":path:[]      -> startServerOn $ SockAddrUNIX path
        "ipv4":addr:port:[] -> startServerOn $ SockAddrInet4 (read addr) (read port)
        "ipv6":addr:port:[] -> startServerOn $ SockAddrInet6 (read addr) (read port)
        _ -> usage

usage :: IO ()
usage = do
    putStrLn "  client unix <filepath>"
    putStrLn "  client ipv4 <address> <port>"
    putStrLn "  client ipv6 <address> <port>"
