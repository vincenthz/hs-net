{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Net.Types
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

instance Arbitrary IPv4Addr where
    arbitrary = ipv4 <$> ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary IPv6Addr where
    arbitrary = ipv6 <$> ((,,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

tests = testGroup "net-types"
    [ testGroup "ipv4"
        [ testProperty "read . show == id" $ \(ip :: IPv4Addr) -> read (show ip) == ip
        ]
    , testGroup "ipv6"
        [ testProperty "read . show == id" $ \(ip :: IPv6Addr) -> read (show ip) == ip
        ]
    ]

main = defaultMain tests
