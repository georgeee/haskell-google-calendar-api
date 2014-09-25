{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Google.Calendar.Calendar (module Network.Google.Calendar.Calendar, module CEntities) where

import           GHC.Generics
import           Data.List (intercalate)

import           Network.Google.ApiIO
import           Network.Google.ApiIO.Generators
import           Network.Google.ApiIO.Common (urlEncode)
import           Network.Google.Calendar.MethodCommon

import           Network.Google.Calendar.Entities
import           Network.Google.Calendar.Calendar.Entities as CEntities
import           Network.HTTP.Types              (StdMethod(..))

mkUrl = mkUrl' . (["calendars"] ++ )
mkIdUrl calendarId = mkUrl [ urlEncode calendarId ]
mkIdUrl' calendarId = mkUrl . (urlEncode calendarId :)
mkEUrl = mkUrl []


-- Method clear

data ClearParams = ClearParams { cpCalendarId :: CalendarId }
                                deriving (Generic, Show)
instance AdditionalParams ClearParams where
    apMethod _ = POST
    apUrl = flip mkIdUrl' ["clear"] . cpCalendarId

clearParams = DefaultRequestParams () . ClearParams

clear :: DefaultMethodTag () ClearParams ()
clear = fullScopeMethod

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

get :: DefaultMethodTag () GetParams Calendar
get = readOnlyScopeMethod

-- Method insert

data InsertParams = InsertParams { ipRequestBody :: InsertableCalendar }
                                deriving (Generic, Show)
instance AdditionalParams InsertParams where
    apMethod _ = POST 
    apUrl _ = mkEUrl
    apBody = resourceBody . ipRequestBody

insertParams = DefaultRequestParams () . InsertParams

insert :: DefaultMethodTag () InsertParams Calendar
insert = fullScopeMethod

-- Method patch

-- @TODO Implement patch method

-- Method update

data UpdateParams = UpdateParams { upCalendarId :: CalendarId , upRequestBody :: UpdatableCalendar }
                                deriving (Generic, Show)
instance AdditionalParams UpdateParams where
    apMethod _ = PUT
    apUrl = mkIdUrl . upCalendarId
    apBody = resourceBody . upRequestBody

updateParams calendarId body = DefaultRequestParams () $ UpdateParams calendarId body

update :: DefaultMethodTag () UpdateParams Calendar
update = fullScopeMethod
