-- |
-- Module      : Net.Socket.System.Error
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE DeriveDataTypeable #-}
module Net.Socket.System.Error
    ( SocketFamilyNotAvailable(..)
    , SocketTypeNotAvailable(..)
    ) where

import Control.Exception
import Data.Typeable

data SocketFamilyNotAvailable = SocketFamilyNotAvailable String
    deriving (Show,Typeable)

data SocketTypeNotAvailable = SocketTypeNotAvailable String
    deriving (Show,Typeable)

instance Exception SocketFamilyNotAvailable
instance Exception SocketTypeNotAvailable

