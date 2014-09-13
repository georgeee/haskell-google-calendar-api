{-# LANGUAGE DeriveGeneric     #-}
module GoogleCalendarAPIRequest where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch             (MonadThrow)

import           Data.Char (toLower)
import           Data.Aeson                      (encode, decode)
import           Data.Aeson.Types                as AT
import           Data.Aeson.Types                (FromJSON)
import           Data.Aeson.TH                   (defaultOptions, Options(..))
import           GHC.Generics

import           Data.Monoid
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           Data.DateTime
import           Data.Time.RFC3339
import           System.Locale

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as B8
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Char8      as L8
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text.Lazy.Encoding         as LT
import qualified Data.Text.Lazy.IO               as LT
import           Data.HashMap.Strict             (HashMap(..))

import           Network.OAuth.OAuth2            (AccessToken, authenticatedRequest, handleResponse, OAuth2Result)

import           Network.HTTP.Client             (Manager, RequestBody(..), Request, parseUrl)
import qualified Network.HTTP.Client             as H
import           Network.HTTP.Types              (StdMethod(..), HeaderName)
import           Network.HTTP.QueryString        as Q


import qualified Data.CaseInsensitive            as CI
import           GHC.Generics

import           GoogleCalendarAPIEntities

class ApiRequestParams a where
    requestQueryParams :: a -> [(String, String)]
    requestQueryParams = const []

    requestBody :: a -> Maybe AT.Value
    requestBody = const Nothing

    requestUrlBase :: a -> String

    requestMethod :: a -> StdMethod
    requestMethod x |isJust (requestBody x)  = POST
                    |otherwise               = GET
    
class (ApiRequestParams a) => ModifiableApiRequestParams a where
    setRequestParam :: String -> String -> a -> a

class (ApiRequestParams a) => PagableRequestParams a where
    setPageToken :: PageToken -> a -> a

processRequest' :: (ApiRequestParams rT, FromJSON a) => Manager -> AccessToken -> rT -> IO (OAuth2Result a)
processRequest' manager token reqParams = liftM parseResponseJSON $ processRequestLBS manager token reqParams

processRequest :: (ApiRequestParams rT, FromJSON a) => Manager -> AccessToken -> rT -> IO a
processRequest m t r = processRequest' m t r >>= either (fail . ("OAuth2 error : " ++) . L8.unpack) return

replacePair (n, v) = ((n, v) :) . filter (\(x, _) -> x /= n)

replacePairs :: (Eq a) => [(a, b)] -> [(a, b)] -> [(a, b)]
replacePairs = foldr replacePair

processRequestLBS :: (ApiRequestParams rT) => Manager -> AccessToken -> rT -> IO (OAuth2Result L.ByteString)
processRequestLBS manager token reqParams = do request <- composeRequest reqParams
                                               response <- authenticatedRequest manager token (requestMethod reqParams) request
                                               return $ handleResponse response
                             where composeRequest reqParams = do initReq <- parseUrl $ requestUrlBase reqParams
                                                                 return $ let body' = RequestBodyLBS . encode
                                                                              headers' = replacePairs headersForReplace $ H.requestHeaders initReq
                                                                              qs' = modifyQS (H.queryString initReq) (requestQueryParams reqParams)
                                                                              req' b = initReq{ H.requestBody = body' b
                                                                                              , H.requestHeaders = headers'
                                                                                              , H.queryString = qs'
                                                                                              }
                                                                           in  maybe initReq req' $ requestBody reqParams
                                   headersForReplace' = [("Content-Type", "application/json")]
                                   headersForReplace = map (\(n, v) -> (CI.mk $ B8.pack n, B8.pack v)) headersForReplace'

modifyQS :: B8.ByteString -> [(String, String)] -> B8.ByteString
modifyQS orig ps = let orig' = maybe mempty id $ Q.parseQuery orig                                                        
                       ps'   = Q.queryString $ map (\(x, y) -> (B8.pack x, B8.pack y)) ps
                   in Q.toString $ orig' `mappend` ps'

parseResponseJSON :: FromJSON a => OAuth2Result L.ByteString -> OAuth2Result a
parseResponseJSON (Left b) = Left b
parseResponseJSON (Right b) = case decode b of
                            Nothing -> Left (L8.pack "Could not decode JSON" `L.append` b)
                            Just x -> Right x

class PagableResponse a where
    nextPageToken :: a -> Maybe PageToken
    nextPageToken = const Nothing

    mergeResponses :: a -> a -> a


data CalendarListListResponse = CalendarListListResponse { cllrEtag :: ETag
                                                         , cllrNextPageToken :: Maybe PageToken
                                                         , cllrNextSyncToken :: Maybe SyncToken
                                                         , cllrItems :: [CalendarListEntry]
                                                         }
                                deriving (Generic, Show)
instance FromJSON CalendarListListResponse where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "cllr" ""
instance PagableResponse CalendarListListResponse where 
     nextPageToken = cllrNextPageToken
     mergeResponses a b = b { cllrItems = cllrItems a ++ cllrItems b }

calendarAPIBase = "https://www.googleapis.com/calendar/v3/"

data CalendarListListRequestParams = CalendarListListRequestParams { cllrpMaxResults :: Maybe Int
                                                                   , cllrpMinAccessRole :: Maybe CalendarAccessRole
                                                                   , cllrpPageToken :: Maybe PageToken
                                                                   , cllrpShowDeleted :: Maybe Bool
                                                                   , cllrpShowHidden :: Maybe Bool
                                                                   , cllrpSyncToken :: Maybe SyncToken
                                                                   }

instance ApiRequestParams CalendarListListRequestParams where
    requestUrlBase = const $ calendarAPIBase ++ "users/me/calendarList"
    -- @TODO add generic params
instance PagableRequestParams CalendarListListRequestParams where
    setPageToken token reqParams = reqParams { cllrpPageToken = Just token }
    -- @TODO add generic pageToken



calendarListList m t r = processRequest m t r :: IO CalendarListListResponse

iterateOverPages :: (PagableRequestParams rT, PagableResponse a) => (rT -> IO a) -> rT -> IO a
iterateOverPages func = iter'
                            where iter resp reqParams = case nextPageToken resp of
                                                             Nothing -> return resp
                                                             Just tok -> liftM (mergeResponses resp) $ iter' $ setPageToken tok reqParams
                                  iter' reqParams = do resp <- func reqParams
                                                       iter resp reqParams



-- @TODO abstract result merging (all methods except Events.list provide the same mechanism)
