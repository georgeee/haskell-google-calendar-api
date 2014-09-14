{-# LANGUAGE DeriveGeneric, TemplateHaskell   #-}
module Network.Google.Calendar.Entities where

import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Types                as AT
import           GHC.Generics

import           Data.Time
import           Data.Time.RFC3339
import           System.Locale

import qualified Data.Text                       as T
import           Data.HashMap.Strict             (HashMap(..))

import           Network.Google.Calendar.Common
import           Network.Google.Calendar.Entities.Internal

type Location = String
type ETag = String
type CalendarId = String
type ApiTimeZone = String
type CalendarColorId = String
type PageToken = String
type SyncToken = String
type Email = String
type EventICalUID = String
type PersonId = String
type EventColorId = String
type EventRecurrenceRule = String
type EventId = String

newtype Color = Color String
    deriving (Generic, Show)
instance FromJSON Color
instance ToJSON Color

newtype EventDate = EventDate Day
    deriving (Generic, Show)
instance FromJSON EventDate where
     parseJSON (String text) = returnMaybe $ parsedDay
                           where returnMaybe (Just day) = return $ EventDate day
                                 returnMaybe _ = mzero
                                 parsedDay = parseTime defaultTimeLocale "%F" $ T.unpack text :: Maybe Day
instance ToJSON EventDate where
     toJSON (EventDate day) = AT.String $ T.pack $ formatTime defaultTimeLocale "%F" day

data ApiDateTime = ApiDateTime LocalTime | ApiDateTimeZoned ZonedTime
    deriving (Generic, Show)
instance FromJSON ApiDateTime where
     parseJSON (String timeText) = case tryZoned timeString of
                                        (Just zonedTime) -> return $ ApiDateTimeZoned zonedTime
                                        Nothing -> case tryLocal timeString of
                                                        (Just localTime) -> return $ ApiDateTime localTime
                                                        Nothing -> mzero
                                   where tryZoned = readRFC3339
                                         tryLocal = parseTime defaultTimeLocale "%FT%T" :: String -> Maybe LocalTime
                                         timeString = T.unpack timeText
instance ToJSON ApiDateTime where
     toJSON (ApiDateTime localTime) = AT.String $ T.pack $ formatTime defaultTimeLocale "%FT%T" localTime
     toJSON (ApiDateTimeZoned zonedTime) = AT.String $ T.pack $ formatTime defaultTimeLocale "%FT%T%z" zonedTime


data CalendarNotificationMethod = CNMEmail | CNMSms
    deriving (Generic, Show)
$(generateEntityInstances "" "CNM" ''CalendarNotificationMethod)

data CalendarNotificationType = CNTEventCreation | CNTEventChange | CNTEventCancellation | CNTEventResponse | CNTAgenda
   deriving (Generic, Show)
$(generateEntityInstances "" "CNT" ''CalendarNotificationType)

data CalendarNotification = CalendarNotification {
    cnMethod :: CalendarNotificationMethod,
    cnType :: CalendarNotificationType
}
    deriving (Generic, Show)
$(generateEntityInstances "cn" "" ''CalendarNotification)

data CalendarNotificationSettings = CalendarNotificationSettings {
    cnsNotifications :: [CalendarNotification]
}
    deriving (Generic, Show)
$(generateEntityInstances "cns" "" ''CalendarNotificationSettings)

data CRMethod = CRMEmail | CRMSms | CRMPopup
                deriving (Generic, Show)
$(generateEntityInstances "" "CRM" ''CRMethod)
                  
data Reminder = Reminder { crMethod :: CRMethod
                         , crMinutes :: Int
                         }
                        deriving (Generic, Show)
$(generateEntityInstances "cr" "" ''Reminder)


data CalendarAccessRole = CARFreeBusyReader | CARReader | CARWriter | CAROwner
                        deriving (Generic, Show)
$(generateEntityInstances "" "CAR" ''CalendarAccessRole)
instance ToString CalendarAccessRole where
    toString = removePrefixLCFirst "CAR" . show



data CalendarListEntry = CalendarListEntry { cleEtag :: ETag
                                           , cleId :: CalendarId
                                           , cleSummary :: T.Text
                                           , cleDesc :: Maybe T.Text
                                           , cleLocation :: Maybe Location
                                           , cleTimeZone :: Maybe ApiTimeZone
                                           , cleSummaryOverride :: Maybe T.Text
                                           , cleColorId :: Maybe CalendarColorId
                                           , cleBackgroundColor :: Maybe Color
                                           , cleForegroundColor :: Maybe Color
                                           , cleHidden :: Maybe Bool
                                           , cleSelected :: Maybe Bool
                                           , cleAccessRole :: CalendarAccessRole
                                           , cleDefaultReminders :: Maybe [Reminder]
                                           , cleNotificationSettings :: Maybe CalendarNotificationSettings
                                           , clePrimary :: Maybe Bool
                                           , cleDeleted :: Maybe Bool
                                           }
    deriving (Generic, Show)
$(generateEntityInstances "cle" "" ''CalendarListEntry)

data InsertableCalendarListEntry = InsertableCalendarListEntry { icleId :: CalendarId
                                                               , icleSummaryOverride :: Maybe T.Text
                                                               , icleColorId :: Maybe CalendarColorId
                                                               , icleBackgroundColor :: Maybe Color
                                                               , icleForegroundColor :: Maybe Color
                                                               , icleHidden :: Maybe Bool
                                                               , icleSelected :: Maybe Bool
                                                               , icleDefaultReminders :: Maybe [Reminder]
                                                               , icleNotificationSettings :: Maybe CalendarNotificationSettings
                                                               }
    deriving (Generic, Show)
$(generateEntityInstances "icle" "" ''InsertableCalendarListEntry)

data UpdatableCalendarListEntry = UpdatableCalendarListEntry { ucleSummaryOverride :: Maybe T.Text
                                                             , ucleColorId :: Maybe CalendarColorId
                                                             , ucleBackgroundColor :: Maybe Color
                                                             , ucleForegroundColor :: Maybe Color
                                                             , ucleHidden :: Maybe Bool
                                                             , ucleSelected :: Maybe Bool
                                                             , ucleDefaultReminders :: Maybe [Reminder]
                                                             , ucleNotificationSettings :: Maybe CalendarNotificationSettings
                                                             }
    deriving (Generic, Show)
$(generateEntityInstances "ucle" "" ''UpdatableCalendarListEntry)

-- @TODO clean input types (E.g. Event), Maybe Bool => Bool, Maybe [] -> [], remove ToJSON instance (check that no use)

data Calendar = Calendar { cEtag :: ETag
                         , cId :: CalendarId
                         , cSummary :: T.Text
                         , cDescription :: Maybe T.Text
                         , cLocation :: Maybe Location
                         , cTimeZone :: Maybe ApiTimeZone
                         }
                    deriving (Generic, Show)
$(generateEntityInstances "c" "" ''Calendar)

data InsertableCalendar = InsertableCalendar { icSummary :: T.Text }
                    deriving (Generic, Show)
$(generateEntityInstances "ic" "" ''InsertableCalendar)

data UpdatableCalendar = UpdatableCalendar { ucSummary :: Maybe T.Text
                                           , ucDescription :: Maybe T.Text
                                           , ucLocation :: Maybe Location
                                           , ucTimeZone :: Maybe ApiTimeZone
                                           }
                    deriving (Generic, Show)
$(generateEntityInstances "uc" "" ''UpdatableCalendar)

data EventTransparency = ETOpaque | ETTransparent
    deriving (Generic, Show)
$(generateEntityInstances "" "ET" ''EventTransparency)

data EventReminders = EventReminders { erUseDefault :: Bool
                                     , erOverrides  :: Maybe [Reminder]
                                     }
    deriving (Generic, Show)
$(generateEntityInstances "er" "" ''EventReminders)

data EventSource = EventSource { esUrl :: String, esTitle :: String }
    deriving (Generic, Show)
$(generateEntityInstances "es" "" ''EventSource)

data EventVisibility = EVDefault | EVPublic | EVPrivate | EVConfidential
    deriving (Generic, Show)
$(generateEntityInstances "" "EV" ''EventVisibility)

data EventExtendedProps = EventExtendedProps { eepPrivate :: HashMap String String
                                             , eepShared  :: HashMap String String
                                             }
    deriving (Generic, Show)
$(generateEntityInstances "eep" "" ''EventExtendedProps)

data EAResponseStatus = EASNeedsAction | EASDeclined | EASTentative | EASAccepted
    deriving (Generic, Show)
$(generateEntityInstances "" "EAS" ''EAResponseStatus)

data EventAttendee = EventAttendee { eaId :: Maybe PersonId
                                   , eaEmail :: Maybe Email 
                                   , eaDisplayName :: Maybe String
                                   , eaOrganizer :: Maybe Bool
                                   , eaSelf :: Maybe Bool
                                   , eaResource :: Maybe Bool
                                   , eaOptional :: Maybe Bool
                                   , eaResponseStatus :: EAResponseStatus
                                   , eaComment :: Maybe String
                                   , eaAdditionalGuests :: Maybe Int
                                   }
    deriving (Generic, Show)
$(generateEntityInstances "ea" "" ''EventAttendee)

data WritableEventAttendee = WritableEventAttendee { weaEmail :: Email
                                                   , weaDisplayName :: Maybe String
                                                   , weaOptional :: Maybe Bool
                                                   , weaResponseStatus :: Maybe EAResponseStatus
                                                   , weaComment :: Maybe String
                                                   , weaAdditionalGuests :: Maybe Int
                                                   }
    deriving (Generic, Show)
$(generateEntityInstances "wea" "" ''WritableEventAttendee)

data EventStatus = EConfirmed | ETentative | ECancelled
    deriving (Generic, Show)
$(generateEntityInstances "" "E" ''EventStatus)

data EventPerson = EventPerson { pId :: Maybe PersonId
                               , pEmail :: Maybe Email
                               , pDisplayName :: Maybe String
                               , pSelf :: Maybe Bool
                               }
    deriving (Generic, Show)
$(generateEntityInstances "p" "" ''EventPerson)

data InsertableEventPerson = InsertableEventPerson { wpEmail :: Maybe Email
                                                   , wpDisplayName :: Maybe String
                                                   }
    deriving (Generic, Show)
$(generateEntityInstances "wp" "" ''InsertableEventPerson)

data EventDateTimeObject = EventDateTimeObject { edtDate :: Maybe EventDate
                                               , edtDateTime :: Maybe ApiDateTime
                                               , edtTimeZone :: Maybe ApiTimeZone
                                               }
    deriving (Generic, Show)
$(generateEntityInstances "edt" "" ''EventDateTimeObject)

data Event = Event { eEtag :: ETag
                   , eId :: EventId
                   , eStatus :: Maybe EventStatus
                   , eHtmlLink :: String
                   , eCreated :: ApiDateTime
                   , eUpdated :: ApiDateTime
                   , eSummary :: T.Text
                   , eDescription :: Maybe T.Text
                   , eLocation :: Maybe Location
                   , eColorId :: Maybe EventColorId
                   , eCreator :: EventPerson
                   , eOrganizer :: EventPerson
                   , eStart :: EventDateTimeObject
                   , eEnd :: EventDateTimeObject
                   , eEndTimeSpecified :: Maybe Bool
                   , eRecurrence :: Maybe [EventRecurrenceRule]
                   , eRecurringEventId :: Maybe EventId
                   , eOriginalStartTime :: Maybe EventDateTimeObject
                   , eTransparency :: Maybe EventTransparency 
                   , eVisibility :: Maybe EventVisibility
                   , eICalUID :: EventICalUID
                   , eSequence :: Int
                   , eAttendees :: Maybe [EventAttendee]
                   , eAttendeesOmitted :: Maybe Bool
                   , eExtendedProperties :: Maybe EventExtendedProps
                   , eHangoutLink :: Maybe String
                   -- , eGadget :: (undefined)
                   , eAnyoneCanAddSelf :: Maybe Bool
                   , eGuestsCanInviteOthers :: Maybe Bool
                   , eGuestsCanModify :: Maybe Bool
                   , eGuestsCanSeeOtherGuests :: Maybe Bool
                   , ePrivateCopy :: Maybe Bool
                   , eLocked :: Maybe Bool
                   , eReminders :: EventReminders
                   , eSource :: Maybe EventSource
                   }
    deriving (Generic, Show)
$(generateEntityInstances "e" "" ''Event)

data UpdatableEvent = UpdatableEvent { ueStatus :: Maybe EventStatus
                                     , ueSummary :: Maybe T.Text
                                     , ueDescription :: Maybe T.Text
                                     , ueLocation :: Maybe Location
                                     , ueColorId :: Maybe EventColorId
                                     , ueStart :: EventDateTimeObject
                                     , ueEnd :: EventDateTimeObject
                                     , ueRecurrence :: Maybe [EventRecurrenceRule]
                                     , ueOriginalStartTime :: Maybe EventDateTimeObject
                                     , ueTransparency :: Maybe EventTransparency 
                                     , ueVisibility :: Maybe EventVisibility
                                     , ueAttendees :: Maybe [WritableEventAttendee]
                                     , ueAttendeesOmitted :: Maybe Bool
                                     , ueExtendedProperties :: Maybe EventExtendedProps
                                     --u , eGadget :: (undefined)
                                     , ueAnyoneCanAddSelf :: Maybe Bool
                                     , ueGuestsCanInviteOthers :: Maybe Bool
                                     , ueGuestsCanSeeOtherGuests :: Maybe Bool
                                     , ueReminders :: Maybe EventReminders
                                     , ueSequence :: Maybe Int
                                     , ueSource :: Maybe EventSource
                                     }
    deriving (Generic, Show)
$(generateEntityInstances "ue" "" ''UpdatableEvent)
defaultUpdatableEvent start end = UpdatableEvent { ueStatus = Nothing
                                                 , ueSummary = Nothing
                                                 , ueDescription = Nothing
                                                 , ueLocation = Nothing
                                                 , ueColorId = Nothing
                                                 , ueStart = start
                                                 , ueEnd  = end
                                                 , ueRecurrence = Nothing
                                                 , ueOriginalStartTime = Nothing
                                                 , ueTransparency = Nothing
                                                 , ueVisibility = Nothing
                                                 , ueAttendees = Nothing
                                                 , ueAttendeesOmitted = Nothing
                                                 , ueExtendedProperties = Nothing
                                                 , ueAnyoneCanAddSelf = Nothing
                                                 , ueGuestsCanInviteOthers = Nothing
                                                 , ueGuestsCanSeeOtherGuests = Nothing
                                                 , ueReminders = Nothing
                                                 , ueSequence = Nothing
                                                 , ueSource = Nothing
                                                 }

data InsertableEvent = InsertableEvent { ieStatus :: Maybe EventStatus
                                       , ieId :: Maybe EventId
                                       , ieSummary :: Maybe T.Text
                                       , ieDescription :: Maybe T.Text
                                       , ieLocation :: Maybe Location
                                       , ieColorId :: Maybe EventColorId
                                       , ieStart :: EventDateTimeObject
                                       , ieEnd :: EventDateTimeObject
                                       , ieRecurrence :: Maybe [EventRecurrenceRule]
                                       , ieOriginalStartTime :: Maybe EventDateTimeObject
                                       , ieTransparency :: Maybe EventTransparency 
                                       , ieVisibility :: Maybe EventVisibility
                                       , ieAttendees :: Maybe [WritableEventAttendee]
                                       , ieExtendedProperties :: Maybe EventExtendedProps
                                       --u , eGadget :: (undefined)
                                       , ieAnyoneCanAddSelf :: Maybe Bool
                                       , ieGuestsCanInviteOthers :: Maybe Bool
                                       , ieGuestsCanSeeOtherGuests :: Maybe Bool
                                       , ieReminders :: Maybe EventReminders
                                       , ieSequence :: Maybe Int
                                       , ieSource :: Maybe EventSource
                                       }
    deriving (Generic, Show)
$(generateEntityInstances "ie" "" ''InsertableEvent)
defaultInsertableEvent start end = InsertableEvent { ieStatus = Nothing
                                                   , ieId = Nothing
                                                   , ieSummary = Nothing
                                                   , ieDescription = Nothing
                                                   , ieLocation = Nothing
                                                   , ieColorId = Nothing
                                                   , ieStart = start
                                                   , ieEnd  = end
                                                   , ieRecurrence = Nothing
                                                   , ieOriginalStartTime = Nothing
                                                   , ieTransparency = Nothing
                                                   , ieVisibility = Nothing
                                                   , ieAttendees = Nothing
                                                   , ieExtendedProperties = Nothing
                                                   , ieAnyoneCanAddSelf = Nothing
                                                   , ieGuestsCanInviteOthers = Nothing
                                                   , ieGuestsCanSeeOtherGuests = Nothing
                                                   , ieReminders = Nothing
                                                   , ieSequence = Nothing
                                                   , ieSource = Nothing
                                                   }

