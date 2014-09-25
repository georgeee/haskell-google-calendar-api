{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Google.Calendar.Event (module Network.Google.Calendar.Event, module EEntities) where

import           GHC.Generics
import           Data.List (intercalate)

import qualified Data.Text                       as T

import           Network.Google.ApiIO
import           Network.Google.ApiIO.Entities
import           Network.Google.ApiIO.Generators
import           Network.Google.ApiIO.Common (urlEncode)
import           Network.Google.Calendar.MethodCommon

import           Network.Google.Calendar.Entities
import           Network.Google.Calendar.Event.Entities as EEntities
import           Network.HTTP.Types              (StdMethod(..))

mkUrl calendarId = mkUrl' . ([ "calendars" , urlEncode calendarId , "events" ] ++ )
mkIdUrl' calendarId eventId = mkUrl calendarId . (urlEncode eventId :)
mkIdUrl calendarId eventId = mkIdUrl' calendarId eventId []
mkEUrl = flip mkUrl []


-- Method delete

data DeleteParams = DeleteParams { dpCalendarId :: CalendarId, dpEventId :: EventId }
        deriving (Generic, Show)
instance AdditionalParams DeleteParams where
    apMethod _ = DELETE
    apUrl x = mkIdUrl (dpCalendarId x) (dpEventId x)
data DeleteQueryParams = DeleteQueryParams { dpSendNotification :: Maybe Bool }
        deriving (Generic, Show)
$(genQueryParams "dp" ''DeleteQueryParams)
instance DefaultParams DeleteQueryParams where defaultParams = DeleteQueryParams Nothing

deleteParams calendarId eventId = flip DefaultRequestParams $ DeleteParams calendarId eventId

delete :: DefaultMethodTag DeleteQueryParams DeleteParams ()
delete = fullScopeMethod

-- Method get

data GetParams = GetParams { gpCalendarId :: CalendarId, gpEventId :: EventId }
                                deriving (Generic, Show)
instance AdditionalParams GetParams where
    apMethod _ = GET
    apUrl x = mkIdUrl (gpCalendarId x) (gpEventId x)
data GetQueryParams = GetQueryParams { gpAlwaysIncludeEmail :: Maybe Bool
                                     , gpMaxAttendees :: Maybe Int
                                     , gpTimeZone :: Maybe ApiTimeZone
                                     }
        deriving (Generic, Show)
$(genQueryParams "gp" ''GetQueryParams)
instance DefaultParams GetQueryParams where defaultParams = GetQueryParams Nothing Nothing Nothing

getParams calendarId eventId = flip DefaultRequestParams $ GetParams calendarId eventId

get :: DefaultMethodTag GetQueryParams GetParams Event
get = readOnlyScopeMethod

-- Method import

-- @TODO implement import method

-- Method insert

data InsertParams = InsertParams { ipCalendarId :: CalendarId, ipRequestBody :: InsertableEvent }
                                deriving (Generic, Show)
instance AdditionalParams InsertParams where
    apMethod _ = POST 
    apUrl = mkEUrl . ipCalendarId
    apBody = resourceBody . ipRequestBody
data InsertQueryParams = InsertQueryParams { ipSendNotification :: Maybe Bool
                                           , ipMaxAttendees :: Maybe Int
                                           }
                                deriving (Generic, Show)
$(genQueryParams "ip" ''InsertQueryParams)
instance DefaultParams InsertQueryParams where
    defaultParams = InsertQueryParams Nothing Nothing

insertParams calendarId qp = DefaultRequestParams qp . InsertParams calendarId

insert :: DefaultMethodTag InsertQueryParams InsertParams Event
insert = fullScopeMethod

-- Method instances

-- @TODO implement instances method

-- Method list
data ListResponse = ListResponse { lrEtag :: ETag
                                 , lrSummary :: T.Text
                                 , lrDescription :: Maybe T.Text
                                 , lrUpdated :: ApiDateTime
                                 , lrTimeZone :: ApiTimeZone
                                 , lrAccessRole :: CalendarAccessRole
                                 , lrDefaultReminders :: Maybe [Reminder]
                                 , lrNextPageToken :: Maybe PageToken
                                 , lrNextSyncToken :: Maybe SyncToken
                                 , lrItems :: [Event]
                                 }
                                deriving (Generic, Show)
$(genPagableResponse "lr" ''ListResponse)
$(genRecJSONInstances "lr" ''ListResponse)

data ListQueryParams = ListQueryParams { lpAlwaysInsludeEmail :: Maybe Bool
                                       , lpICalUID :: Maybe EventICalUID
                                       , lpMaxAttendees :: Maybe Int
                                       , lpMaxResults :: Maybe Int
                                       , lpOrderBy :: Maybe EventOrderBy
                                       , lpPageToken :: Maybe PageToken
                                       -- @TODO implement (need better than ToString implementation), lpPrivateExtendedProperty :: [(String, String)] 
                                       , lpQ :: Maybe T.Text
                                       , lpShowDeleted :: Maybe Bool
                                       , lpShowHiddenInvitations :: Maybe Bool
                                       , lpSingleEvents :: Maybe Bool
                                       , lpSyncToken :: Maybe SyncToken
                                       , lpTimeMax :: Maybe ApiDateTime
                                       , lpTimeMin :: Maybe ApiDateTime
                                       , lpUpdateMin :: Maybe ApiDateTime
                                       }
                                deriving (Generic, Show)
$(genQueryParams "lp" ''ListQueryParams)
instance DefaultParams ListQueryParams where
    defaultParams = ListQueryParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data ListParams = ListParams { lpCalendarId :: CalendarId } deriving (Show)
instance AdditionalParams ListParams where apUrl = mkEUrl . lpCalendarId

listParams calendarId = flip DefaultRequestParams $ ListParams calendarId

list :: DefaultMethodTag ListQueryParams ListParams ListResponse
list = readOnlyScopeMethod

-- Method move

-- @TODO Implement method move

-- Method patch

-- @TODO Implement method patch

-- Method quickAdd

data QuickAddParams = QuickAddParams { qapCalendarId :: CalendarId } deriving Show
instance AdditionalParams QuickAddParams where
    apMethod _ = POST
    apUrl = flip mkUrl [ "quickAdd" ] . qapCalendarId

data QuickAddQueryParams = QuickAddQueryParams { qapText :: T.Text
                                               , qapSendNotifications :: Maybe Bool
                                               }
                                deriving (Generic, Show)
$(genQueryParams "qap" ''QuickAddQueryParams)

quickAddQueryParams text = QuickAddQueryParams text Nothing
quickAddParams calendarId = flip DefaultRequestParams $ QuickAddParams calendarId

quickAdd :: DefaultMethodTag QuickAddQueryParams QuickAddParams Event
quickAdd = fullScopeMethod

-- Method update

data UpdateParams = UpdateParams { upCalendarId :: CalendarId
                                 , upEventId :: EventId
                                 , upRequestBody :: UpdatableEvent }
                                deriving (Generic, Show)
instance AdditionalParams UpdateParams where
    apMethod _ = PUT
    apUrl x = mkIdUrl (upCalendarId x) (upEventId x)
    apBody = resourceBody . upRequestBody

data UpdateQueryParams = UpdateQueryParams { upAlwaysIncludeEmail :: Maybe Bool
                                           , upMaxAttendees :: Maybe Int
                                           , upSendNotification :: Maybe Bool
                                           }
                                deriving (Generic, Show)
$(genQueryParams "up" ''UpdateQueryParams)
instance DefaultParams UpdateQueryParams where
    defaultParams = UpdateQueryParams Nothing Nothing Nothing

updateParams calendarId eventId body = flip DefaultRequestParams $ UpdateParams calendarId eventId body

update :: DefaultMethodTag UpdateQueryParams UpdateParams Event
update = fullScopeMethod

-- Method watch

-- @TODO Implement watch method
