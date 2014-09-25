{-# LANGUAGE DeriveGeneric, TemplateHaskell   #-}
module Network.Google.Calendar.Event.Entities where

import qualified Data.Text                       as T
import           Network.Google.Calendar.Entities
import           Network.Google.ApiIO.Entities
import           Network.Google.ApiIO.GenericParams (ToString(..))
import           Network.Google.ApiIO.Generators (genJSONInstances, genRecJSONInstances, genTagJSONInstances)
import           GHC.Generics
import           Data.HashMap.Strict             (HashMap(..))

data EventOrderBy = OrderByStartTime | OrderByUpdated
    deriving (Generic, Show)
instance ToString EventOrderBy where
    toString OrderByUpdated = "updated"
    toString OrderByStartTime = "startTime"

data EventTransparency = ETOpaque | ETTransparent
    deriving (Generic, Show)
$(genTagJSONInstances "ET" ''EventTransparency)

data EventReminders = EventReminders { erUseDefault :: Bool
                                     , erOverrides  :: Maybe [Reminder]
                                     }
    deriving (Generic, Show)
$(genRecJSONInstances "er" ''EventReminders)

data EventSource = EventSource { esUrl :: String, esTitle :: String }
    deriving (Generic, Show)
$(genRecJSONInstances "es" ''EventSource)

data EventVisibility = EVDefault | EVPublic | EVPrivate | EVConfidential
    deriving (Generic, Show)
$(genTagJSONInstances "EV" ''EventVisibility)

data EventExtendedProps = EventExtendedProps { eepPrivate :: HashMap String String
                                             , eepShared  :: HashMap String String
                                             }
    deriving (Generic, Show)
$(genRecJSONInstances "eep" ''EventExtendedProps)

data EAResponseStatus = EASNeedsAction | EASDeclined | EASTentative | EASAccepted
    deriving (Generic, Show)
$(genTagJSONInstances "EAS" ''EAResponseStatus)

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
$(genRecJSONInstances "ea" ''EventAttendee)

data WritableEventAttendee = WritableEventAttendee { weaEmail :: Email
                                                   , weaDisplayName :: Maybe String
                                                   , weaOptional :: Maybe Bool
                                                   , weaResponseStatus :: Maybe EAResponseStatus
                                                   , weaComment :: Maybe String
                                                   , weaAdditionalGuests :: Maybe Int
                                                   }
    deriving (Generic, Show)
$(genRecJSONInstances "wea" ''WritableEventAttendee)

data EventStatus = EConfirmed | ETentative | ECancelled
    deriving (Generic, Show)
$(genTagJSONInstances "E" ''EventStatus)

data EventPerson = EventPerson { pId :: Maybe PersonId
                               , pEmail :: Maybe Email
                               , pDisplayName :: Maybe String
                               , pSelf :: Maybe Bool
                               }
    deriving (Generic, Show)
$(genRecJSONInstances "p" ''EventPerson)

data InsertableEventPerson = InsertableEventPerson { wpEmail :: Maybe Email
                                                   , wpDisplayName :: Maybe String
                                                   }
    deriving (Generic, Show)
$(genRecJSONInstances "wp" ''InsertableEventPerson)

data EventDateTimeObject = EventDateTimeObject { edtDate :: Maybe ApiDate
                                               , edtDateTime :: Maybe ApiDateTime
                                               , edtTimeZone :: Maybe ApiTimeZone
                                               }
    deriving (Generic, Show)
$(genRecJSONInstances "edt" ''EventDateTimeObject)

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
$(genRecJSONInstances "e" ''Event)

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
$(genRecJSONInstances "ue" ''UpdatableEvent)
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
                                       -- , ieGadget :: (undefined)
                                       , ieAnyoneCanAddSelf :: Maybe Bool
                                       , ieGuestsCanInviteOthers :: Maybe Bool
                                       , ieGuestsCanSeeOtherGuests :: Maybe Bool
                                       , ieReminders :: Maybe EventReminders
                                       , ieSequence :: Maybe Int
                                       , ieSource :: Maybe EventSource
                                       }
    deriving (Generic, Show)
$(genRecJSONInstances "ie" ''InsertableEvent)
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
