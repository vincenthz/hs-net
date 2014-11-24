import Arbitrary.URI

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit

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
    | otherwise = error $ "assertEq: value are not equal\nx1(" ++ show x1 ++ ")\nx2(" ++ show x2 ++ ")"

listOfURIHUnits :: [ (String, URI) ]
listOfURIHUnits =
    [ ( "http://google.com"
      , URI (URIScheme "http") (Just $ URIAuth (UserInfo "") (HostName "google.com" Nothing)) (URIPath "") (URIQuery "") (URIFragment "")
      )
    , ( "https://www.facebook.com/haskell"
      , URI (URIScheme "https") (Just $ URIAuth (UserInfo "") (HostName "www.facebook.com" Nothing)) (URIPath "/haskell") (URIQuery "") (URIFragment "")
      )
    , ( "https://www.haskell.org/haskellwiki/Typeclassopedia#Monad"
      , URI (URIScheme "https") (Just $ URIAuth (UserInfo "") (HostName "www.haskell.org" Nothing)) (URIPath "/haskellwiki/Typeclassopedia") (URIQuery "") (URIFragment "Monad")
      )
    ]

hunit_decode_encode_uri :: String -> URI -> Assertion
hunit_decode_encode_uri strUri uri =
    case uriFromString strUri of
        Left err -> error $ "unable to decode the encoded uri: " ++ err
        Right uri' ->
            if assertEq uri uri'
                then return ()
                else assertFailure "the uri is not the one expected"

prop_encode_decode_uri :: ArbitraryURI -> Bool
prop_encode_decode_uri aUri =
    let uri = getURI aUri
        strUri = uriToString uri
    in  case uriFromString strUri of
            Left err -> error $ "unable to decode the encoded uri: " ++ err
            Right uri' -> assertEq uri uri'

main :: IO ()
main = defaultMain tests
