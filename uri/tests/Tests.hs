module Main where

import Arbitrary.URI

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit
import Test.QuickCheck

import Net.URI

tests =
    [ testGroup "units"
        $ map (\(str, uri) -> testCase str $ hunit_decode_encode_uri str uri) listOfURIHUnits
    , testGroup "properties"
        [ testProperty "parse" prop_encode_decode_uri
        ]
    ]

assertEq :: (Show a, Eq a)
         => a
         -> a
         -> Bool
assertEq x1 x2
    | x1 == x2  = True
    | otherwise = error $ "assertEq: value are not equal x1(" ++ show x1 ++ ") /= x2(" ++ show x2 ++ ")"

listOfURIHUnits :: [ (String, URI) ]
listOfURIHUnits =
    [ ( "http://google.com"
      , URI "http:" (Just $ URIAuth "" "google.com" "") "" "" ""
      )
    , ( "https://www.facebook.com/haskell"
      , URI "https:" (Just $ URIAuth "" "www.facebook.com" "") "/haskell" "" ""
      )
    , ( "https://www.haskell.org/haskellwiki/Typeclassopedia#Monad"
      , URI "https:" (Just $ URIAuth "" "www.haskell.org" "") "/haskellwiki/Typeclassopedia" "" "#Monad"
      )
    ]

hunit_decode_encode_uri :: String -> URI -> Assertion
hunit_decode_encode_uri strUri uri =
    case parseURI strUri of
        Nothing   -> error $ "unable to decode the encoded uri: " ++ show strUri
        Just uri' ->
            if assertEq uri uri'
                then return ()
                else assertFailure "the uri is not the one expected"

prop_encode_decode_uri :: ArbitraryURI -> Bool
prop_encode_decode_uri aUri =
    let uri = getURI aUri
        strUri = show uri
    in  case parseURI strUri of
            Nothing   -> error $ "unable to decode the encoded uri: " ++ show strUri
            Just uri' -> assertEq uri uri'

main :: IO ()
main = defaultMain tests
