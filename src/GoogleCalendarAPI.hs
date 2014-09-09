{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
module GoogleCalendarAPI where

import           Control.Applicative
import           Control.Monad

import           Data.Char (toLower)
import           Data.Aeson
import           Data.Aeson.Types                as AT
import           Data.Aeson.TH
import           GHC.Generics

import           Data.Time
import           Data.Time.Format
import           Data.DateTime
import           Data.Time.RFC3339
import           System.Locale

import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import qualified Data.ByteString.Lazy            as L
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text.Lazy.Encoding         as LT
import qualified Data.Text.Lazy.IO               as LT
import           Data.HashMap.Strict             (HashMap(..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Method       (methodGet, methodPost, StdMethod(GET, POST))


import           Network.HTTP.Types              (hContentType)


import GoogleCalendarAPITypes



