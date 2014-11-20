module Arbitrary.URI
    ( ArbitraryURI(..)
    ) where

import Control.Applicative
import Data.List

import Test.QuickCheck

import Net.URI

printURI :: URI -> String
printURI uri =
    "URI\n"
      ++ "  scheme:   " ++ (show $ uriScheme uri) ++ "\n"
      ++ "  auth:     " ++ (show $ uriAuthority uri) ++ "\n"
      ++ "  path:     " ++ (show $ uriPath uri) ++ "\n"
      ++ "  query:    " ++ (show $ uriQuery uri) ++ "\n"
      ++ "  fragment: " ++ (show $ uriFragment uri) ++ "\n"

newtype ArbitraryURI = ArbitraryURI
    { getURI :: URI
    } deriving (Eq)

instance Show ArbitraryURI where
    show (ArbitraryURI uri) = "ArbitraryURI: " ++ printURI uri

instance Arbitrary ArbitraryURI where
    arbitrary = ArbitraryURI <$> generateURI

generateURI :: Gen URI
generateURI =
    URI <$> generateScheme
        <*> oneof [ return Nothing, Just <$> generateURIAuth ]
        <*> generatePath
        <*> generateQuery
        <*> generateFragment

generateURIAuth :: Gen URIAuth
generateURIAuth =
    URIAuth <$> generateUserInfo
            <*> generateRegName
            <*> generatePort

generateUserInfo :: Gen String
generateUserInfo =
    elements [ ""
             , "@"
             , "user@"
             , "user:@"
             , "user:...@"
             , "user:...@"
             ]

generateRegName :: Gen String
generateRegName = do
    numNodes <- choose (0, 3)
    lNodes <- vectorOf numNodes $ listOf1 $ oneof [ choose ('a', 'z'), choose ('0', '9')]
    sizeRoot <- choose (2, 5)
    root <- vectorOf sizeRoot $ choose ('a', 'z')
    return $ intercalate "" (lNodes ++ [root])

generatePort :: Gen String
generatePort =
    frequency [ (75, (\n -> ':' : (show n)) <$> (choose (0, 8080) :: Gen Int))
              , (25, return "" )]


generateScheme :: Gen String
generateScheme = elements [ "http:", "https:", "ftp:", "ssh:", "ftps:" ]

generatePath :: Gen String
generatePath = do
    numNodes <- choose (0, 3)
    lNodes <- vectorOf numNodes $ listOf1 $ oneof [ choose ('a', 'z'), choose ('0', '9')]
    sizeRoot <- choose (2, 5)
    root <- vectorOf sizeRoot $ choose ('a', 'z')
    return $ '/' : (intercalate "/" (lNodes ++ [root]))

generateQuery :: Gen String
generateQuery = do
    numQueries <- choose (0, 2)
    lQueries <- vectorOf numQueries $ listOf1 $ oneof [ choose ('a', 'z'), choose ('0', '9'), return '=']
    return $ '?' : (intercalate "&" lQueries)

generateFragment :: Gen String
generateFragment = do
    sizeFragment <- choose (0, 10)
    frag <- vectorOf sizeFragment $ oneof [ choose ('a', 'z'), choose ('0', '9')]
    return $ '#' : frag

