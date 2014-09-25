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
mkIdUrl calendarId = mkUrl [ urlEncode calendarId ]
mkEUrl = mkUrl []
-- Method delete

data DeleteParams = DeleteParams { dpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams DeleteParams where
    apMethod _ = DELETE
    apUrl = mkIdUrl . dpCalendarId

deleteParams = DefaultRequestParams () . DeleteParams

delete :: DefaultMethodTag () DeleteParams ()
delete = fullScopeMethod

-- Method get

data GetParams = GetParams { gpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams GetParams where
    apMethod _ = GET
    apUrl = mkIdUrl . gpCalendarId

getParams = DefaultRequestParams () . GetParams

get :: DefaultMethodTag () GetParams CalendarLE
get = readOnlyScopeMethod

-- Method insert

data InsertParams = InsertParams { ipRequestBody :: InsertableCalendarLE }
                                deriving (Generic, Show)
instance AdditionalParams InsertParams where
    apMethod _ = POST 
    apUrl _ = mkEUrl
    apBody = resourceBody . ipRequestBody
data InsertQueryParams = InsertQueryParams { ipColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(genQueryParams "ip" ''InsertQueryParams)
instance DefaultParams InsertQueryParams where
    defaultParams = InsertQueryParams Nothing

insertParams qp = DefaultRequestParams qp . InsertParams

insert :: DefaultMethodTag InsertQueryParams InsertParams CalendarLE
insert = fullScopeMethod

-- Method list

data ListResponse = ListResponse { lrEtag :: ETag
                                 , lrNextPageToken :: Maybe PageToken
                                 , lrNextSyncToken :: Maybe SyncToken
                                 , lrItems :: [CalendarLE]
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
instance AdditionalParams ListParams where apUrl _ = mkEUrl

listParams qp = DefaultRequestParams qp ListParams

list :: DefaultMethodTag ListQueryParams ListParams ListResponse
list = readOnlyScopeMethod


-- Method patch

-- @TODO Implement patch method

-- Method update

data UpdateParams = UpdateParams { upCalendarId :: CalendarId , upRequestBody :: UpdatableCalendarLE }
                                deriving (Generic, Show)
instance AdditionalParams UpdateParams where
    apMethod _ = PUT
    apUrl = mkIdUrl . upCalendarId
    apBody = resourceBody . upRequestBody

data UpdateQueryParams = UpdateQueryParams { upColorRgbFormat :: Maybe Bool }
                                deriving (Generic, Show)
$(genQueryParams "up" ''UpdateQueryParams)
instance DefaultParams UpdateQueryParams where
    defaultParams = UpdateQueryParams Nothing

updateParams calendarId body qp = DefaultRequestParams qp $ UpdateParams calendarId body

update :: DefaultMethodTag UpdateQueryParams UpdateParams CalendarLE
update = fullScopeMethod

-- Method watch

-- @TODO Implement watch method
