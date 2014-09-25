{-# LANGUAGE DeriveGeneric #-}
module Network.Google.ApiIO.Entities where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types                as AT
import           GHC.Generics

import           Data.Time
import           Data.Time.RFC3339
import           System.Locale

import qualified Data.Text                       as T
import           Network.Google.ApiIO.GenericParams (ToString(..))

newtype ApiDate = ApiDate Day
    deriving (Show)
instance FromJSON ApiDate where
     parseJSON (String text) = decodeDate $ T.unpack text
     parseJSON _ = mzero
instance ToJSON ApiDate where
     toJSON = AT.String . T.pack . encodeDate
instance ToString ApiDate where
    toString = encodeDate

encodeDate :: ApiDate -> String
encodeDate (ApiDate day) = formatTime defaultTimeLocale "%F" day

decodeDate :: MonadPlus m => String -> m ApiDate
decodeDate dayString = returnMaybe $ parsedDay
                   where returnMaybe (Just day) = return $ ApiDate day
                         returnMaybe _ = mzero
                         parsedDay = parseTime defaultTimeLocale "%F" dayString :: Maybe Day


data ApiDateTime = ApiDateTime LocalTime | ApiDateTimeZoned ZonedTime
    deriving (Show)
instance FromJSON ApiDateTime where
     parseJSON (String t) = decodeTime $ T.unpack t
     parseJSON _ = mzero
instance ToJSON ApiDateTime where
     toJSON = AT.String . T.pack . encodeTime
instance ToString ApiDateTime where
    toString = encodeTime

encodeTime :: ApiDateTime -> String
encodeTime (ApiDateTime localTime) = formatTime defaultTimeLocale "%FT%T" localTime
encodeTime (ApiDateTimeZoned zonedTime) = formatTime defaultTimeLocale "%FT%T%z" zonedTime

decodeTime :: MonadPlus m => String -> m ApiDateTime
decodeTime timeString = case tryZoned timeString of
                           (Just zonedTime) -> return $ ApiDateTimeZoned zonedTime
                           Nothing -> case tryLocal timeString of
                                           (Just localTime) -> return $ ApiDateTime localTime
                                           Nothing -> mzero
                      where tryZoned = readRFC3339
                            tryLocal = parseTime defaultTimeLocale "%FT%T" :: String -> Maybe LocalTime

