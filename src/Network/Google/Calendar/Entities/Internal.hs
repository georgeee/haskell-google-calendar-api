{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Network.Google.Calendar.Entities.Internal where
import           Data.Aeson.Types                as AT
import           Language.Haskell.TH

import           Network.Google.Calendar.Common

removePrefixLCFirstOpts flPrefix ctPrefix = defaultOptions { fieldLabelModifier = removePrefixLCFirst flPrefix
                                                           , constructorTagModifier = removePrefixLCFirst ctPrefix
                                                           , omitNothingFields = True
                                                           }
generateEntityFromJSONInstance flPrefix ctPrefix typeName = [d|
                                        instance FromJSON $(conT typeName) where
                                             parseJSON = genericParseJSON $ removePrefixLCFirstOpts flPrefix ctPrefix
                                        |]
generateEntityToJSONInstance flPrefix ctPrefix typeName = [d|
                                        instance ToJSON $(conT typeName) where
                                             toJSON = genericToJSON $ removePrefixLCFirstOpts flPrefix ctPrefix
                                        |]

generateEntityInstances flPrefix ctPrefix typeName = concatM [ generateEntityFromJSONInstance flPrefix ctPrefix typeName
                                                             , generateEntityToJSONInstance flPrefix ctPrefix typeName
                                                             ]
