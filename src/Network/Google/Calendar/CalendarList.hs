{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Google.Calendar.CalendarList where

import           GHC.Generics
import           Data.Aeson (toJSON)
import           Data.List (intercalate)

import           Network.Google.ApiIO
import           Network.Google.ApiIO.Generators
import           Network.Google.Calendar.MethodCommon

import           Network.Google.Calendar.Entities
import           Network.Google.Calendar.CalendarList.Entities
import           Network.HTTP.Types              (StdMethod(..))

mkUrl = mkUrl' . ("calendarList" : )

-- Method delete

data DeleteResponse = DeleteResponse
                                deriving (Generic, Show)
$(genRecJSONInstances "dr" ''DeleteResponse)

data DeleteParams = DeleteParams { dpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams DeleteParams where
    apMethod _ = DELETE
    apUrl x = mkUrl [ "delete", dpCalendarId x ]
data DeleteQueryParams = DeleteQueryParams
instance QueryParams DeleteQueryParams

delete :: DefaultMethodTag DeleteQueryParams DeleteParams DeleteResponse
delete = fullScopeMethod

-- Method get

data GetParams = GetParams { gpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams GetParams where
    apMethod _ = GET 
    apUrl x = mkUrl [ "get", gpCalendarId x ]
data GetQueryParams = GetQueryParams
instance QueryParams GetQueryParams

get :: DefaultMethodTag GetQueryParams GetParams Entry
get = readOnlyScopeMethod

-- Method insert

data InsertParams = InsertParams { ipRequestBody :: InsertableEntry }
                                deriving (Generic, Show)
instance AdditionalParams InsertParams where
    apUrl _ = mkUrl []
    apBody = Just . toJSON . ipRequestBody
data InsertQueryParams = InsertQueryParams { ipColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(genQueryParams "ip" ''InsertQueryParams)

insert :: DefaultMethodTag InsertQueryParams InsertParams Entry
insert = fullScopeMethod

-- Method list

data ListResponse = ListResponse { lrEtag :: ETag
                                 , lrNextPageToken :: Maybe PageToken
                                 , lrNextSyncToken :: Maybe SyncToken
                                 , lrItems :: [Entry]
                                 }
                                deriving (Generic, Show)
$(genPagableResponse "lr" ''ListResponse)
$(genRecJSONInstances "lr" ''ListResponse)

data ListQueryParams = ListQueryParams { lpMaxResults :: Maybe Int
                                       , lpMinAccessRole :: Maybe CalendarAccessRole
                                       , lpPageToken :: Maybe PageToken
                                       , lpShowDeleted :: Maybe Bool
                                       , lpShowHidden :: Maybe Bool
                                       , lpSyncToken :: Maybe SyncToken
                                       }
                                deriving (Generic, Show)
$(genQueryParams "lp" ''ListQueryParams)
instance DefaultParams ListQueryParams where
    defaultParams = ListQueryParams Nothing Nothing Nothing Nothing Nothing Nothing

data ListParams = ListParams
instance AdditionalParams ListParams where apUrl _ = mkUrl [ "list" ]

list :: DefaultMethodTag ListQueryParams ListParams ListResponse
list = readOnlyScopeMethod

