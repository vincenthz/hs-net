module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Net.Socket

import System.Environment
import System.Exit

startServerOn :: SockAddr addr => SocketType -> addr -> IO ()
startServerOn socketType addr = do
    s <- connect addr socketType
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
        "unix":path:[]        -> startServerOn Stream     $ SockAddrUNIX path
        "ipv4":t:addr:port:[] -> startServerOn (toType t) $ SockAddrInet4 (read addr) (read port)
        "ipv6":t:addr:port:[] -> startServerOn (toType t) $ SockAddrInet6 (read addr) (read port)
        _ -> usage
  where
    toType :: String -> SocketType
    toType str = case str of
        "udp" -> Datagram
        "tcp" -> Stream
        _     -> error $ "Socket Type type not found " ++ show str

usage :: IO ()
usage = do
    putStrLn "  client unix <filepath>"
    putStrLn "  client [tcp|udp] ipv4 <address> <port>"
    putStrLn "  client [tcp|udp] ipv6 <address> <port>"
