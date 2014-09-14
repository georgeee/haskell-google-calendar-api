{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Google.Calendar.Methods where

import           GHC.Generics
import           Data.Aeson (toJSON)

import           Network.Google.Calendar.Entities         as E
import           Network.Google.Calendar.Methods.Internal
import           Network.Google.Calendar.Api
import           Network.Google.Calendar.Common
import           Network.HTTP.Types              (StdMethod(..))
import           Network.Google.Calendar.Methods.GenericParams           as G

class DefaultParams a where
    defaultParams :: a

-- Method calendarList/list

data CalendarListListResponse = CalendarListListResponse { cllrEtag :: ETag
                                                         , cllrNextPageToken :: Maybe PageToken
                                                         , cllrNextSyncToken :: Maybe SyncToken
                                                         , cllrItems :: [CalendarListEntry]
                                                         }
                                deriving (Generic, Show)
$(generateResponseInstances "cllr" ''CalendarListListResponse)

data CalendarListListRequestParams = CalendarListListRequestParams { cllrpMaxResults :: Maybe Int
                                                                   , cllrpMinAccessRole :: Maybe CalendarAccessRole
                                                                   , cllrpPageToken :: Maybe PageToken
                                                                   , cllrpShowDeleted :: Maybe Bool
                                                                   , cllrpShowHidden :: Maybe Bool
                                                                   , cllrpSyncToken :: Maybe SyncToken
                                                                   }
                                deriving (Generic, Show)
$(generateRequestInstances "users/me/calendarList" "cllrp" ''CalendarListListRequestParams)
instance DefaultParams CalendarListListRequestParams where
    defaultParams = CalendarListListRequestParams Nothing Nothing Nothing Nothing Nothing Nothing

calendarListList :: MethodTag CalendarListListRequestParams CalendarListListResponse
calendarListList = readOnlyScopeMethod

-- Method calendarList/delete

data CalendarListDeleteResponse = CalendarListDeleteResponse { }
                                deriving (Generic, Show)
$(generateJSONResponseInstances "cldr" ''CalendarListDeleteResponse)

data CalendarListDeleteRequestParams = CalendarListDeleteRequestParams { cldrpCalendarId :: CalendarId }
                                deriving (Generic, Show)
$(generatePathRequestParamsInstance DELETE "calendarList/delete" [| cldrpCalendarId |] ''CalendarListDeleteRequestParams)

calendarListDelete :: MethodTag CalendarListDeleteRequestParams CalendarListDeleteResponse
calendarListDelete = fullScopeMethod


-- Method calendarList/get

type CalendarListGetResponse = CalendarListEntry

data CalendarListGetRequestParams = CalendarListGetRequestParams { clgrpCalendarId :: CalendarId }
                                deriving (Generic, Show)
$(generatePathRequestParamsInstance GET "calendarList/get" [| clgrpCalendarId |] ''CalendarListGetRequestParams)

calendarListGet :: MethodTag CalendarListGetRequestParams CalendarListGetResponse
calendarListGet = readOnlyScopeMethod


-- Method calendarList/insert

type CalendarListInsertResponse = CalendarListEntry

data CalendarListInsertRequestParams = CalendarListInsertRequestParams { clirpRequestBody :: IgnoredParam InsertableCalendarListEntry
                                                                       , clirpColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(generateApiRequestParamsInstance "calendarList/insert" [| Just . toJSON . ignoredParamValue . clirpRequestBody |] "clirp" ''CalendarListInsertRequestParams)

calendarListInsert :: MethodTag CalendarListInsertRequestParams CalendarListInsertResponse
calendarListInsert = fullScopeMethod


