{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Network.Google.ApiIO.Generators where

import           Data.Aeson.Types
import           Network.HTTP.Types              (StdMethod(..))
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift        (deriveLift)

import qualified Network.Google.ApiIO.GenericParams           as G
import           Network.Google.ApiIO.Types
import           Network.Google.ApiIO.Common

-- pageTokenSuffix = "PageToken"


$(deriveLift ''StdMethod)

removePrefixLCFirstOpts fieldPrefix constructorPrefix = defaultOptions { fieldLabelModifier = removePrefixLCFirst fieldPrefix
                                                           , constructorTagModifier = removePrefixLCFirst constructorPrefix
                                                           , omitNothingFields = True
                                                           }
genFromJSON fieldPrefix constructorPrefix typeName = [d|
                                        instance FromJSON $(conT typeName) where
                                             parseJSON = genericParseJSON $ removePrefixLCFirstOpts fieldPrefix constructorPrefix
                                        |]
genToJSON fieldPrefix constructorPrefix typeName = [d|
                                        instance ToJSON $(conT typeName) where
                                             toJSON = genericToJSON $ removePrefixLCFirstOpts fieldPrefix constructorPrefix
                                        |]

genJSONInstances fieldPrefix constructorPrefix typeName = concatM [ genFromJSON fieldPrefix constructorPrefix typeName
                                                                  , genToJSON fieldPrefix constructorPrefix typeName
                                                                  ]

genPagableResponse' :: String -> String ->  String -> Name -> Q [Dec]
genPagableResponse' nextPageTokenSuffix itemsSuffix prefix typeName = [d|
                                         instance PagableResponse $(conT typeName) where
                                            nextPageToken = $(nextPageToken)
                                            mergeResponses a b = $(recUpdE [|b|] [fieldExp itemsName [| $(items) a ++ $(items) b |] ])
                                        |]
                                      where name = mkName . (prefix ++)
                                            nextPageToken = varE $ name nextPageTokenSuffix
                                            itemsName = name itemsSuffix
                                            items = varE $ itemsName
genPagableResponse = genPagableResponse' "NextPageToken" "Items"

genRecFromJSON = flip genFromJSON ""
genRecToJSON = flip genToJSON ""
genRecJSONInstances = flip genJSONInstances ""

genTagFromJSON = genFromJSON ""
genTagToJSON = genToJSON ""
genTagJSONInstances = genJSONInstances ""

genQueryParams :: String -> Name -> Q [Dec]
genQueryParams prefix typeName = [d|
                                   instance QueryParams $(conT typeName) where
                                       queryParams = G.genericParams (G.removePrefixLCFirstOpts prefix)
                                 |]
