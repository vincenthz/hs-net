-- |
-- Module      : Net.URI.Types
-- License     : BSD-Style
-- Copyright   : Copyright © 2014, Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Net.URI.Types
    ( URI(..)
    , URIScheme(..)
    , URIPath(..)
    , URIQuery(..)
    , URIFragment(..)
    , URIAuth(..)
    , HostName(..)
    , UserInfo(..)
    ) where

newtype URIScheme = URIScheme
    { getURIScheme :: String
    } deriving (Show, Eq)

newtype URIPath = URIPath
    { getURIPath :: String
    } deriving (Show, Eq)

newtype URIQuery = URIQuery
    { getURIQuery :: String
    } deriving (Show, Eq)

newtype URIFragment = URIFragment
    { getURIFragment :: String
    } deriving (Show, Eq)

data HostName = HostName
    { getAddress :: String
    , getPort    :: Maybe Int
    } deriving (Show, Eq)

newtype UserInfo = UserInfo String
    deriving (Show, Eq)

data URIAuth = URIAuth
    { uriUserInfo :: UserInfo
    , uriHostName :: HostName
    } deriving (Show, Eq)

data URI = URI
    { uriScheme    :: URIScheme
    , uriAuthority :: Maybe URIAuth
    , uriPath      :: URIPath
    , uriQuery     :: URIQuery
    , uriFragment  :: URIFragment
    } deriving (Show, Eq)
