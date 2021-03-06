module Network.Google.Calendar.MethodCommon where

import           Data.Aeson (toJSON, ToJSON, Value, FromJSON)
import           Data.List (intercalate)
import           Network.Google.ApiIO (ApiRequestParams, methodTag, MethodTag)
import           Network.Google.Calendar.Constraints

mkUrl' = intercalate "/" . ( calendarAPIUrlBase : )

readOnlyScopeMethod :: (ApiRequestParams rT, FromJSON b) => MethodTag rT b   
readOnlyScopeMethod = methodTag [readOnlyScope, fullScope]

fullScopeMethod :: (ApiRequestParams rT, FromJSON b) => MethodTag rT b   
fullScopeMethod = methodTag [fullScope]

resourceBody :: (ToJSON a) => a -> Maybe Value
resourceBody = Just . toJSON

