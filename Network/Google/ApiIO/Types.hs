module Network.Google.ApiIO.Types where

import           Network.OAuth.OAuth2            (OAuth2Result)
import qualified Data.ByteString.Lazy            as L
import           Network.HTTP.Types (StdMethod(..))
import           Data.Aeson.Types
import           Data.Maybe
import           Control.Monad.Free

type AuthScope = String
type PageToken = String
type SyncToken = String

class ApiRequestParams a where
    requestQueryParams :: a -> [(String, String)]
    requestQueryParams _ = []

    requestBody :: a -> Maybe Value
    requestBody = const Nothing

    requestUrlBase :: a -> String

    requestMethod :: a -> StdMethod
    requestMethod x |isJust (requestBody x)  = POST
                    |otherwise               = GET


class (ApiRequestParams a) => PagableRequestParams a where
    setPageToken :: PageToken -> a -> a

class PagableResponse a where
    nextPageToken :: a -> Maybe PageToken
    nextPageToken = const Nothing

    mergeResponses :: a -> a -> a

class QueryParams a where
    queryParams :: a -> [(String, String)]
    queryParams _ = []

class AdditionalParams a where
    apBody :: a -> Maybe Value
    apBody _ = Nothing

    apUrl :: a -> String
    
    apMethod :: a -> StdMethod
    apMethod x |isJust (apBody x)  = POST
               |otherwise          = GET

instance QueryParams () where
    queryParams _ = []

data DefaultRequestParams qp ap = DefaultRequestParams qp ap
    deriving Show

instance (QueryParams a, AdditionalParams b) => ApiRequestParams (DefaultRequestParams a b) where
    requestQueryParams (DefaultRequestParams q _) = queryParams q
    requestBody (DefaultRequestParams _ a) = apBody a
    requestUrlBase (DefaultRequestParams _ a) = apUrl a
    requestMethod (DefaultRequestParams _ a) = apMethod a

class DefaultParams a where
    defaultParams :: a

data ApiIOF arg f = RawRequest arg (OAuth2Result L.ByteString -> f)
instance Functor (ApiIOF arg) where
    fmap f (RawRequest r k) = RawRequest r (f . k)
type ApiIO arg = Free (ApiIOF arg)
data MethodTag paramsT respT = MethodTag { authScope :: [AuthScope]
                                         , method :: paramsT -> ApiIO paramsT respT
                                         }

type DefaultMethodTag queryPs addPs = MethodTag (DefaultRequestParams queryPs addPs)
