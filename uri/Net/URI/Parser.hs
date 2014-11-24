-- |
-- Module      : Net.URI.Parser
-- License     : BSD-Style
-- Copyright   : Copyright © 2014, Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Net.URI.Parser
    ( parserURI
    ) where

import           Control.Applicative
import           Data.String.Parse (Parser)
import qualified Data.String.Parse as P
import           Net.URI.Types

parserURI :: Parser URI
parserURI = do
    scheme   <- parserURIScheme
    P.string "://"
    mauth    <- parserMaybeURIAuth
    path     <- parserURIPath
    query    <- parserURIQuery
    fragment <- parserURIFragment
    return $ URI scheme mauth path query fragment

-- | read until empty or ':'
parserURIScheme :: Parser URIScheme
parserURIScheme =
    URIScheme
        <$> P.takeWhile ((/=) ':')

parserMaybeURIAuth :: Parser (Maybe URIAuth)
parserMaybeURIAuth = do
    c <- P.peekAnyChar
    if c `elem` "/?#"
        then return Nothing -- in this case there is no URIAuth
        else Just <$> parserURIAuth

parserURIAuth :: Parser URIAuth
parserURIAuth =
    (URIAuth <$> parserUserInfo <*> parserHostName)
    <|> (URIAuth (UserInfo "") <$> parserHostName)

-- username ':' passwd '@'
parserUserInfo :: Parser UserInfo
parserUserInfo = do
    res <- UserInfo <$> P.takeWhile ((/=) '@')
    P.char '@'
    return res

-- ipv4 | '[' ipv6 ']' | domain name
-- [ ':' port ]
parserHostName :: Parser HostName
parserHostName =
    (HostName <$> parserDomainAddress <*> (P.char ':' >> Just . read <$> P.takeWhileToEnd ((/=) '/')))
    <|> (HostName <$> parserDomainAddress <*> return Nothing)

parserDomainAddress :: Parser String
parserDomainAddress =
    (P.char '[' >> P.takeWhile ((/=) ']') >>= \str -> P.char ']' >> return str) -- read an ipv6
    <|> (P.takeWhileToEnd (not . flip elem "/:"))

-- | read until empty or '?' or '#'
parserURIPath :: Parser URIPath
parserURIPath =
    URIPath
        <$> ((P.char '/' >> ((:) '/') <$> P.takeWhileToEnd (not . flip elem "#?")) <|> (return ""))

-- | read until empty or '#'
parserURIQuery :: Parser URIQuery
parserURIQuery =
    URIQuery
        <$> ((P.char '?' >> P.takeWhileToEnd ((/=) '#')) <|> (return ""))

-- | read until empty
parserURIFragment :: Parser URIFragment
parserURIFragment = do
    URIFragment
        <$> ((P.char '#' >> P.takeWhileToEnd (\_ -> True)) <|> (return ""))
