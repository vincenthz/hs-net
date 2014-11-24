module Arbitrary.URI
    ( ArbitraryURI(..)
    ) where

import Control.Applicative
import Data.List

import Test.QuickCheck

import Net.URI

newtype ArbitraryURI = ArbitraryURI
    { getURI :: URI
    } deriving (Eq, Show)

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
            <*> generateHostName

generateHostName :: Gen HostName
generateHostName =
    HostName <$> generateRegName
             <*> oneof [ return Nothing, Just <$> choose (0, 9095) ]

generateUserInfo :: Gen UserInfo
generateUserInfo =
    UserInfo
        <$> elements [ ""
                     , "user"
                     , "user:"
                     , "user:..."
                     , "user:..."
                     ]

generateRegName :: Gen String
generateRegName = do
    numNodes <- choose (0, 3)
    lNodes <- vectorOf numNodes $ listOf1 $ oneof [ choose ('a', 'z'), choose ('0', '9')]
    sizeRoot <- choose (2, 5)
    root <- vectorOf sizeRoot $ choose ('a', 'z')
    return $ intercalate "" (lNodes ++ [root])

generateScheme :: Gen URIScheme
generateScheme =
    URIScheme
        <$> elements [ "http", "https", "ftp", "ssh", "ftps" ]

generatePath :: Gen URIPath
generatePath = do
    numNodes <- choose (1, 4)
    lNodes <- vectorOf numNodes $ listOf1 $ oneof [ choose ('a', 'z'), choose ('0', '9')]
    sizeRoot <- choose (2, 5)
    root <- vectorOf sizeRoot $ choose ('a', 'z')
    return $ URIPath $ if numNodes == 0
        then ""
        else '/' : (intercalate "/" (lNodes ++ [root]))

generateQuery :: Gen URIQuery
generateQuery = do
    numQueries <- choose (0, 2)
    lQueries <- vectorOf numQueries $ listOf1 $ oneof [ choose ('a', 'z'), choose ('0', '9'), return '=']
    return $ URIQuery $ if numQueries == 0
        then ""
        else intercalate "&" lQueries

generateFragment :: Gen URIFragment
generateFragment = do
    sizeFragment <- choose (0, 10)
    frag <- vectorOf sizeFragment $ oneof [ choose ('a', 'z'), choose ('0', '9')]
    return $ URIFragment $ frag
