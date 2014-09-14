{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Network.Google.Calendar.Methods.Internal where

import           Data.Aeson.Types
import           Data.Maybe
import           Data.DateTime
import           Network.HTTP.Types              (StdMethod(..))
import           Language.Haskell.TH

import           Network.Google.Calendar.Entities                        as E
import           Network.Google.Calendar.Entities.Internal               as E
import           Network.Google.Calendar.Methods.GenericParams           as G
import           Network.Google.Calendar.Common
import           Network.Google.Calendar.Constraints

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

generatePagableResponseInstance :: String -> Name -> Q [Dec]
generatePagableResponseInstance prefix typeName = [d|
                                         instance PagableResponse $(conT typeName) where
                                            nextPageToken = $(nextPageToken)
                                            mergeResponses a b = $(recUpdE [|b|] [fieldExp itemsName [| $(items) a ++ $(items) b |] ])
                                        |]
                                      where name = mkName . (prefix ++)
                                            nextPageToken = varE $ name "NextPageToken"
                                            itemsName = name "Items"
                                            items = varE $ itemsName
generateFromJSONInstance prefix typeName = [d|
                                        instance FromJSON $(conT typeName) where
                                             parseJSON = genericParseJSON $ E.removePrefixLCFirstOpts prefix ""
                                        |]
generateToJSONInstance prefix typeName = [d|
                                        instance ToJSON $(conT typeName) where
                                             toJSON = genericToJSON $ E.removePrefixLCFirstOpts prefix ""
                                        |]

generatePagableResponseInstances prefix typeName = concatM [ generateFromJSONInstance prefix typeName
                                                           , generateToJSONInstance prefix typeName
                                                           , generatePagableResponseInstance prefix typeName
                                                           ]

generatePagableRequestParamsInstance :: [Char] -> Name -> Q [Dec]
generatePagableRequestParamsInstance prefix typeName = [d|
                                         instance PagableRequestParams $(conT typeName) where
                                            setPageToken tok reqParams = $(recUpdE [|reqParams|] [fieldExp tokenName [|Just tok|] ])
                                        |]
                                      where name = mkName . (prefix ++)
                                            tokenName = name "PageToken"


generateApiRequestParamsInstance :: [Char] -> Q Exp -> [Char] -> Name -> Q [Dec]
generateApiRequestParamsInstance url bodyQ prefix typeName = [d|
                                        instance ApiRequestParams $(conT typeName) where
                                            requestQueryParams = genericParams $ G.removePrefixLCFirstOpts prefix
                                            requestBody _ = $(bodyQ)
                                            requestUrlBase _ = url
                                        |]
                                
generatePagableRequestInstances url prefix typeName = generatePagableRequestInstancesImpl url [|Nothing|] prefix typeName
generatePagableRequestInstancesWithBody url bodyQ prefix typeName = generatePagableRequestInstancesImpl url [|Just $(bodyQ)|] prefix typeName
generatePagableRequestInstancesImpl url bodyQ prefix typeName = concatM decs
                                                                where url' = calendarAPIUrlBase ++ url
                                                                      decs = [ generateApiRequestParamsInstance url' bodyQ prefix typeName
                                                                             , generatePagableRequestParamsInstance prefix typeName ]


