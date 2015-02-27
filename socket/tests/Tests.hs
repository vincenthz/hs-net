{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Net.Types
import Net.Socket
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

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
    , testGroup "PortNumber"
        [ testProperty "read . show == id" (test_readshow_prop :: PortNumber -> Bool)
        ]
    ]

instance Arbitrary SockAddrInet where
    arbitrary = SockAddrInet <$> arbitrary <*> arbitrary
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
        [ testProperty "marshalling" (test_sockaddr_prop :: (SockAddrInet -> Bool))
        ]
    , testGroup "SockAddrInet6"
        [ testProperty "marshalling" (test_sockaddr_prop :: (SockAddrInet6 -> Bool))
        ]
    , testGroup "SockAddrUnix"
        [ testProperty "marshalling" (test_sockaddr_prop :: (SockAddrUNIX -> Bool))
        ]
    ]

tests = testGroup "net"
    [ testsTypes
    , testsSockAddrs
    ]

main = defaultMain tests
