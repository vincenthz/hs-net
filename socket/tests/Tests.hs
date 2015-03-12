{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Net.Types
import Net.Socket
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Scenario

testEquality :: (Show value, Eq value)
             => value -> value
             -> Bool
testEquality v1 v2
    | v1 == v2  = True
    | otherwise = error $ (show v1) ++ " /= " ++ (show v2)

instance Arbitrary IPv4Addr where
    arbitrary = ipv4 <$> ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary IPv6Addr where
    arbitrary = ipv6 <$> ((,,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary IP where
    arbitrary = oneof
        [ IPv4 <$> arbitrary
        , IPv6 <$> arbitrary
        ]

instance Arbitrary PortNumber where
    arbitrary = portnumber <$> choose (0, 65535)

test_readshow_prop :: (Arbitrary value, Read value, Show value, Eq value)
                   => value
                   -> Bool
test_readshow_prop value =
    testEquality value (read $ show value)

testsTypes = testGroup "types"
    [ testGroup "ipv4"
        [ testProperty "read . show == id" (test_readshow_prop :: IPv4Addr -> Bool)
        ]
    , testGroup "ipv6"
        [ testProperty "read . show == id" (test_readshow_prop :: IPv6Addr -> Bool)
        ]
    , testGroup "ip"
        [ testProperty "read . show == id" (test_readshow_prop :: IP -> Bool)
        ]
    , testGroup "PortNumber"
        [ testProperty "read . show == id" (test_readshow_prop :: PortNumber -> Bool)
        ]
    ]

instance Arbitrary SockAddrInet4 where
    arbitrary = SockAddrInet4 <$> arbitrary <*> arbitrary
instance Arbitrary SockAddrInet6 where
    arbitrary = SockAddrInet6 <$> arbitrary <*> arbitrary
instance Arbitrary SockAddrUNIX where
    arbitrary = SockAddrUNIX <$> (listOf1 $ elements ['a'..'z'])

test_sockaddr_prop :: (Arbitrary addr, SockAddr addr, Eq addr, Show addr)
                   => addr
                   -> Bool
test_sockaddr_prop addr =
    testEquality addr addr' && testEquality addr addr''
  where
    raw = marshalAddr addr
    addr' = unMarshalAddr raw
    raw' = marshalAddr addr'
    addr'' = unMarshalAddr raw'

testsSockAddrs = testGroup "socket"
    [ testGroup "SockAddrInet"
        [ testProperty "marshalling" (test_sockaddr_prop :: (SockAddrInet4 -> Bool))
        ]
    , testGroup "SockAddrInet6"
        [ testProperty "marshalling" (test_sockaddr_prop :: (SockAddrInet6 -> Bool))
        ]
    , testGroup "SockAddrUnix"
        [ testProperty "marshalling" (test_sockaddr_prop :: (SockAddrUNIX -> Bool))
        ]
    ]

testProperties = testGroup "property"
    [ testsTypes
    , testsSockAddrs
    ]

runUnitTest :: (Show addr, SockAddr addr)
            => addr -- ^ Server
            -> addr -- ^ Client
            -> SocketType
            -> IO ()
runUnitTest server client sType = do
    let conf = defaultScenarioConfig server client sType
    res <- startTest conf
    case res of
        Left err -> assertFailure err
        Right _  -> return ()

testUnit :: (Show addr, SockAddr addr)
         => String
         -> addr
         -> addr
         -> SocketType
         -> TestTree
testUnit name server client sType = testCase name (runUnitTest server client sType)

testUnits = testGroup "units"
    [ testUnit "TCP - inet"  (SockAddrInet4  (read "0.0.0.0")  (read "4343")) (SockAddrInet4  (read "127.0.0.1") (read "4343")) Stream
    , testUnit "TCP - inet6" (SockAddrInet6 (read ":::::::0") (read "4344"))  (SockAddrInet6 (read ":::::::1")  (read "4344"))  Stream
    , testUnit "TCP - UNIX"  (SockAddrUNIX  "/tmp/lol.sock.test.tcp")         (SockAddrUNIX "/tmp/lol.sock.test.tcp")           Stream
    , testUnit "UDP - inet"  (SockAddrInet4  (read "0.0.0.0")  (read "4343")) (SockAddrInet4  (read "127.0.0.1") (read "4343")) Datagram
    , testUnit "UDP - inet6" (SockAddrInet6 (read ":::::::0") (read "4344"))  (SockAddrInet6 (read ":::::::1")  (read "4344"))  Datagram
    , testUnit "UDP - UNIX"  (SockAddrUNIX  "/tmp/lol.sock.test.udp")         (SockAddrUNIX "/tmp/lol.sock.test.udp")           Datagram
    ]

main = defaultMain $ testGroup "net"
    [ testProperties
    , testUnits
    ]
