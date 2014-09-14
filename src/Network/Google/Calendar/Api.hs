{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Network.Google.Calendar.Api (ApiIO, processApiIO,
                     rawRequest, request', request, requestBS,
                     processRequest, processRequest', processRawRequest, processRequestBS,
                     iterateOverPages, parseResponseJSON,
                     MethodTag(..), AuthScope(..), readOnlyScopeMethod, fullScopeMethod) where

import           Control.Monad
import           Control.Monad.Free

import           Data.Aeson                      (encode, decode)
import           Data.Aeson.Types                as AT

import           Data.Monoid

import qualified Data.ByteString.Char8           as B8
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Char8      as L8

import           Network.OAuth.OAuth2            (AccessToken, authenticatedRequest, handleResponse, OAuth2Result)
import           Network.HTTP.Client             (Manager, RequestBody(..), parseUrl)
import qualified Network.HTTP.Client             as H
import           Network.HTTP.QueryString        as Q

import qualified Data.CaseInsensitive            as CI

import           Network.Google.Calendar.Methods.Internal
import           Network.Google.Calendar.Common

data ApiIOF arg f = RawRequest arg (OAuth2Result L.ByteString -> f)

instance Functor (ApiIOF arg) where
    fmap f (RawRequest r k) = RawRequest r (f . k)

processApiIOF :: (ApiRequestParams rT) => Manager -> AccessToken -> ApiIOF rT (IO r) -> IO r
processApiIOF m t (RawRequest r f) = processRequestLBS m t r >>= f

processApiIO m t = iterM $ processApiIOF m t
type ApiIO arg = Free (ApiIOF arg)


data AuthScope = ScopeReadOnly | ScopeFull
data MethodTag paramsT respT = MethodTag { authScope :: AuthScope
                                         , apiMethod    :: paramsT -> ApiIO paramsT respT
                                         }

fullScopeMethod :: (ApiRequestParams rT, FromJSON b) => MethodTag rT b
fullScopeMethod = MethodTag ScopeFull request

readOnlyScopeMethod :: (ApiRequestParams rT, FromJSON b) => MethodTag rT b
readOnlyScopeMethod = MethodTag ScopeReadOnly request

rawRequest :: (ApiRequestParams rT) => (OAuth2Result L8.ByteString -> f) -> rT -> ApiIO rT f
rawRequest r = liftF . (flip RawRequest) r

requestBS :: (ApiRequestParams rT) => rT -> ApiIO rT (OAuth2Result L8.ByteString)
requestBS = rawRequest id 

request' :: (ApiRequestParams rT, FromJSON a) => rT -> ApiIO rT (OAuth2Result a)
request' = rawRequest parseResponseJSON

request :: (FromJSON a, ApiRequestParams rT) => rT -> ApiIO rT a
request r = request' r >>= either (fail . ("OAuth2 error : " ++) . L8.unpack) return

processRawRequest m t f = processApiIO m t . rawRequest f
processRequestBS m t = processApiIO m t . requestBS
processRequest' m t = processApiIO m t . request'
processRequest m t = processApiIO m t . request

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

iterateOverPages :: (PagableRequestParams rT, PagableResponse a) => (rT -> ApiIO rT a) -> rT -> ApiIO rT a
iterateOverPages func = iter'
                            where iter resp reqParams = case nextPageToken resp of
                                                             Nothing -> return resp
                                                             Just tok -> liftM (mergeResponses resp) $ iter' $ setPageToken tok reqParams
                                  iter' reqParams = do resp <- func reqParams
                                                       iter resp reqParams

