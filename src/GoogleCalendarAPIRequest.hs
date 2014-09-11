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

import           Network.HTTP.Client             as Http
import           Network.HTTP.Client.TLS         as Http
import           Network.HTTP.Types              as Http

import qualified Data.CaseInsensitive            as CI

class ApiRequestParams a where
    getRequestParams :: a -> [(String, String)]
    getRequestParams = const []

    getRequestBody :: a -> Maybe AT.Value
    getRequestBody = const Nothing

    getRequestUrl :: a -> String

    needAuthorization :: a -> Bool
    needAuthorization = const False
    
    getRequestMethod :: a -> Http.StdMethod
    getRequestMethod x |isJust (getRequestBody x)  = Http.POST
                       |otherwise                  = Http.GET

processRequest :: (ApiRequestParams rT, FromJSON a) => Http.Manager -> AccessToken -> rT -> IO (OAuth2Result a)
processRequest manager token reqParams = liftM parseResponseJSON $ processRequestLBS manager token reqParams

processRequestLBS :: (ApiRequestParams rT) => Http.Manager -> AccessToken -> rT -> IO (OAuth2Result L.ByteString)
processRequestLBS manager token reqParams = do request <- composeRequest reqParams
                                               response <- authenticatedRequest manager token (getRequestMethod reqParams) request
                                               return $ handleResponse response
                             where composeRequest reqParams = do initReq <- parseUrl $ getRequestUrl reqParams
                                                                 let req' = case getRequestBody reqParams of
                                                                            Nothing -> initReq
                                                                            Just body -> initReq{ requestBody = RequestBodyLBS $ encode body
                                                                                                , requestHeaders = replaceHeaders headersForReplace $ requestHeaders initReq
                                                                                                }
                                                                 return req'
                                   headersForReplace' = [("Content-Type", "application/json")]
                                   headersForReplace :: [(Http.HeaderName, B.ByteString)]
                                   headersForReplace = map (\(name, value) -> (CI.mk $ B8.pack name, B8.pack value)) headersForReplace'
                                   replaceHeaders ((name, value):rest) headers = replaceHeaders rest $ (name, value) : filter (\(x, _) -> x /= name) headers
                                   replaceHeaders [] headers = headers

parseResponseJSON :: FromJSON a => OAuth2Result L.ByteString -> OAuth2Result a
parseResponseJSON (Left b) = Left b
parseResponseJSON (Right b) = case decode b of
                            Nothing -> Left (L8.pack "Could not decode JSON" `L.append` b)
                            Just x -> Right x
