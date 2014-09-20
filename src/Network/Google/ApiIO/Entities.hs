{-# LANGUAGE DeriveGeneric #-}
module Network.Google.ApiIO.Entities where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types                as AT
import           GHC.Generics

import           Data.Time
import           Data.Time.RFC3339
import           System.Locale

import qualified Data.Text                       as T

newtype ApiDate = ApiDate Day
    deriving (Generic, Show)
instance FromJSON ApiDate where
     parseJSON (String text) = returnMaybe $ parsedDay
                           where returnMaybe (Just day) = return $ ApiDate day
                                 returnMaybe _ = mzero
                                 parsedDay = parseTime defaultTimeLocale "%F" $ T.unpack text :: Maybe Day
instance ToJSON ApiDate where
     toJSON (ApiDate day) = AT.String $ T.pack $ formatTime defaultTimeLocale "%F" day

data ApiDateTime = ApiDateTime LocalTime | ApiDateTimeZoned ZonedTime
    deriving (Generic, Show)
instance FromJSON ApiDateTime where
     parseJSON (String timeText) = case tryZoned timeString of
                                        (Just zonedTime) -> return $ ApiDateTimeZoned zonedTime
                                        Nothing -> case tryLocal timeString of
                                                        (Just localTime) -> return $ ApiDateTime localTime
                                                        Nothing -> mzero
                                   where tryZoned = readRFC3339
                                         tryLocal = parseTime defaultTimeLocale "%FT%T" :: String -> Maybe LocalTime
                                         timeString = T.unpack timeText
instance ToJSON ApiDateTime where
     toJSON (ApiDateTime localTime) = AT.String $ T.pack $ formatTime defaultTimeLocale "%FT%T" localTime
     toJSON (ApiDateTimeZoned zonedTime) = AT.String $ T.pack $ formatTime defaultTimeLocale "%FT%T%z" zonedTime
