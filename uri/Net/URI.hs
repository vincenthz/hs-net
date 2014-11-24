module Net.URI
    ( -- * URI Types
      URI(..)
    , URIScheme(..)
    , URIPath(..)
    , URIQuery(..)
    , URIFragment(..)
    , URIAuth(..)
    , HostName(..)
    , UserInfo(..)
      -- * Parser
    , parserURI
    , uriFromString

      -- * Printer
    , uriToString
    ) where

import Net.URI.Types
import Net.URI.Parser
import Data.String.Parse (Result(..), parseFeed, parse)

uriFromString :: String -> Either String URI
uriFromString str = do
    e <- parseFeed (return []) parserURI str
    case e of
        ParseFail err -> Left err
        ParseMore _   -> Left "not enough data"
        ParseOK _ uri -> Right uri

uriToString :: URI -> String
uriToString uri =
    scheme ++ "://" ++ auth ++ path ++ query ++ fragment
  where
    scheme :: String
    scheme = getURIScheme $ uriScheme uri

    auth :: String
    auth = maybe "" uriAuthToString $ uriAuthority uri

    path :: String
    path = getURIPath $ uriPath uri

    query :: String
    query =
        case getURIQuery $ uriQuery uri of
            ""  -> ""
            str -> "?" ++ str

    fragment :: String
    fragment =
        case getURIFragment $ uriFragment uri of
            ""  -> ""
            str -> "#" ++ str

uriAuthToString :: URIAuth -> String
uriAuthToString uriauth =
    userinfo ++ hostname
  where
    userinfo :: String
    userinfo =
        case uriUserInfo uriauth of
            UserInfo ""  -> ""
            UserInfo str -> str ++ "@"

    hostname :: String
    hostname =
        addr ++ port
      where
        addr :: String
        addr = getAddress $ uriHostName uriauth

        port :: String
        port = maybe "" (\i -> ":" ++ (show i)) $ getPort $ uriHostName uriauth
