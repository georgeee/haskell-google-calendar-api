{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Google.Calendar.CalendarList (module Network.Google.Calendar.CalendarList, module CLEntities) where

import           GHC.Generics
import           Data.List (intercalate)

import           Network.Google.ApiIO
import           Network.Google.ApiIO.Generators
import           Network.Google.ApiIO.Common (urlEncode)
import           Network.Google.Calendar.MethodCommon

import           Network.Google.Calendar.Entities
import           Network.Google.Calendar.CalendarList.Entities as CLEntities
import           Network.HTTP.Types              (StdMethod(..))

mkUrl = mkUrl' . (["users" , "me" , "calendarList"] ++ )

-- Method delete

data DeleteParams = DeleteParams { dpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams DeleteParams where
    apMethod _ = DELETE
    apUrl x = mkUrl [ urlEncode $ dpCalendarId x ]

deleteParams = DefaultRequestParams () . DeleteParams

delete :: DefaultMethodTag () DeleteParams ()
delete = fullScopeMethod

-- Method get

data GetParams = GetParams { gpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams GetParams where
    apMethod _ = GET
    apUrl x = mkUrl [ urlEncode $ gpCalendarId x ]

getParams = DefaultRequestParams () . GetParams

get :: DefaultMethodTag () GetParams Resource
get = readOnlyScopeMethod

-- Method insert

data InsertParams = InsertParams { ipRequestBody :: InsertableResource }
                                deriving (Generic, Show)
instance AdditionalParams InsertParams where
    apMethod _ = POST 
    apUrl _ = mkUrl []
    apBody = resourceBody . ipRequestBody
data InsertQueryParams = InsertQueryParams { ipColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(genQueryParams "ip" ''InsertQueryParams)
instance DefaultParams InsertQueryParams where
    defaultParams = InsertQueryParams Nothing

insertParams qp = DefaultRequestParams qp . InsertParams

insert :: DefaultMethodTag InsertQueryParams InsertParams Resource
insert = fullScopeMethod

-- Method list

data ListResponse = ListResponse { lrEtag :: ETag
                                 , lrNextPageToken :: Maybe PageToken
                                 , lrNextSyncToken :: Maybe SyncToken
                                 , lrItems :: [Resource]
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

data ListParams = ListParams deriving Show
instance AdditionalParams ListParams where apUrl _ = mkUrl []

listParams qp = DefaultRequestParams qp ListParams

list :: DefaultMethodTag ListQueryParams ListParams ListResponse
list = readOnlyScopeMethod


-- Method patch

data PatchParams = PatchParams { ppCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams PatchParams where
    apMethod _ = PATCH
    apUrl x = mkUrl [ urlEncode $ ppCalendarId x ]

data PatchQueryParams = PatchQueryParams { ppColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(genQueryParams "pp" ''PatchQueryParams)
instance DefaultParams PatchQueryParams where
    defaultParams = PatchQueryParams Nothing

patchParams qp = DefaultRequestParams qp . PatchParams

patch :: DefaultMethodTag PatchQueryParams PatchParams Resource
patch = fullScopeMethod


-- Method update

data UpdateParams = UpdateParams { upCalendarId :: CalendarId , upRequestBody :: UpdatableResource }
                                deriving (Generic, Show)
instance AdditionalParams UpdateParams where
    apMethod _ = PUT
    apUrl x = mkUrl [ urlEncode $ upCalendarId x ]
    apBody = resourceBody . upRequestBody

data UpdateQueryParams = UpdateQueryParams { upColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(genQueryParams "up" ''UpdateQueryParams)
instance DefaultParams UpdateQueryParams where
    defaultParams = UpdateQueryParams Nothing

updateParams qp calendarId body = DefaultRequestParams qp $ UpdateParams calendarId body

update :: DefaultMethodTag UpdateQueryParams UpdateParams Resource
update = fullScopeMethod

-- @TODO watch method
