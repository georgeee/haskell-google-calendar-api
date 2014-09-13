{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
module GoogleCalendarAPIEntities where

import           Control.Applicative
import           Control.Monad

import           Data.Char (toLower)
import           Data.Aeson
import           Data.Aeson.Types                as AT
import           Data.Aeson.TH
import           GHC.Generics

import           Data.Time
import           Data.Time.Format
import           Data.DateTime
import           Data.Time.RFC3339
import           System.Locale

import qualified Data.ByteString.Lazy            as L
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text.Lazy.Encoding         as LT
import qualified Data.Text.Lazy.IO               as LT
import           Data.HashMap.Strict             (HashMap(..))

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
instance FromJSON CalendarListEntry where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "cle" ""

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
instance ToJSON InsertableCalendarListEntry where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "icle" ""

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
instance ToJSON UpdatableCalendarListEntry where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "ucle" ""

-- @TODO insertable, updatable calendar, calendarList + events/calendars responses
-- @TODO move types to own module
-- @TODO apis (hope less time than types)
-- @TODO clean input types (E.g. Event), Maybe Bool => Bool, Maybe [] -> [], remove ToJSON instance (check that no use)

data CalendarNotificationSettings = CalendarNotificationSettings {
    cnsNotifications :: [CalendarNotification]
}
    deriving (Generic, Show)
instance FromJSON CalendarNotificationSettings where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "cns" ""
instance ToJSON CalendarNotificationSettings where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "cns" ""

data CalendarNotification = CalendarNotification {
    cnMethod :: CalendarNotificationMethod,
    cnType :: CalendarNotificationType
}
    deriving (Generic, Show)
instance FromJSON CalendarNotification where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "cn" ""
instance ToJSON CalendarNotification where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "cn" ""

data CalendarNotificationMethod = CNMEmail | CNMSms
    deriving (Generic, Show)
instance FromJSON CalendarNotificationMethod where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "CNM"
instance ToJSON CalendarNotificationMethod where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "CNM"

data CalendarNotificationType = CNTEventCreation | CNTEventChange | CNTEventCancellation | CNTEventResponse | CNTAgenda
                        deriving (Generic, Show)
instance FromJSON CalendarNotificationType where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "CNT"
instance ToJSON CalendarNotificationType where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "CNT"


data Reminder = Reminder { crMethod :: CRMethod
                         , crMinutes :: Int
                         }
                        deriving (Generic, Show)
instance FromJSON Reminder where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "cr" ""
instance ToJSON Reminder where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "cr" ""

data CRMethod = CRMEmail | CRMSms | CRMPopup
                deriving (Generic, Show)
instance FromJSON CRMethod where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "CRM"
instance ToJSON CRMethod where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "CRM"
                  

data CalendarAccessRole = CARFreeBusyReader | CARReader | CARWriter | CAROwner
                        deriving (Generic, Show)
instance FromJSON CalendarAccessRole where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "CAR"
instance ToJSON CalendarAccessRole where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "CAR"

data Calendar = Calendar { cEtag :: ETag
                         , cId :: CalendarId
                         , cSummary :: T.Text
                         , cDescription :: Maybe T.Text
                         , cLocation :: Maybe Location
                         , cTimeZone :: Maybe ApiTimeZone
                         }
                    deriving (Generic, Show)
instance FromJSON Calendar where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "c" ""

data InsertableCalendar = InsertableCalendar { icSummary :: T.Text }
                    deriving (Generic, Show)
instance ToJSON InsertableCalendar where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "ic" ""

data UpdatableCalendar = UpdatableCalendar { ucSummary :: Maybe T.Text
                                           , ucDescription :: Maybe T.Text
                                           , ucLocation :: Maybe Location
                                           , ucTimeZone :: Maybe ApiTimeZone
                                           }
                    deriving (Generic, Show)
instance ToJSON UpdatableCalendar where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "uc" ""

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
instance FromJSON Event where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "e" ""

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
instance ToJSON UpdatableEvent where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "ue" ""
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
instance ToJSON InsertableEvent where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "ie" ""
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

data EventTransparency = ETOpaque | ETTransparent
    deriving (Generic, Show)
instance FromJSON EventTransparency where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "ET"
instance ToJSON EventTransparency where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "ET"

data EventReminders = EventReminders { erUseDefault :: Bool
                                     , erOverrides  :: Maybe [Reminder]
                                     }
    deriving (Generic, Show)
instance FromJSON EventReminders where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "er" ""
instance ToJSON EventReminders where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "er" ""

data EventSource = EventSource { esUrl :: String, esTitle :: String }
    deriving (Generic, Show)
instance FromJSON EventSource where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "es" ""
instance ToJSON EventSource where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "es" ""

data EventVisibility = EVDefault | EVPublic | EVPrivate | EVConfidential
    deriving (Generic, Show)
instance FromJSON EventVisibility where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "EV"
instance ToJSON EventVisibility where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "EV"

data EventExtendedProps = EventExtendedProps { eepPrivate :: HashMap String String
                                             , eepShared  :: HashMap String String
                                             }
    deriving (Generic, Show)
instance FromJSON EventExtendedProps where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "eep" ""
instance ToJSON EventExtendedProps where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "eep" ""

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
instance FromJSON EventAttendee where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "ea" ""
instance ToJSON EventAttendee where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "ea" ""

data WritableEventAttendee = WritableEventAttendee { weaEmail :: Email
                                                   , weaDisplayName :: Maybe String
                                                   , weaOptional :: Maybe Bool
                                                   , weaResponseStatus :: Maybe EAResponseStatus
                                                   , weaComment :: Maybe String
                                                   , weaAdditionalGuests :: Maybe Int
                                                   }
    deriving (Generic, Show)
instance ToJSON WritableEventAttendee where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "wea" ""

data EAResponseStatus = EASNeedsAction | EASDeclined | EASTentative | EASAccepted
    deriving (Generic, Show)
instance FromJSON EAResponseStatus where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "EAS"
instance ToJSON EAResponseStatus where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "EAS"

data EventStatus = EConfirmed | ETentative | ECancelled
    deriving (Generic, Show)
instance FromJSON EventStatus where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "" "E"
instance ToJSON EventStatus where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "" "E"

data EventPerson = EventPerson { pId :: Maybe PersonId
                               , pEmail :: Maybe Email
                               , pDisplayName :: Maybe String
                               , pSelf :: Maybe Bool
                               }
    deriving (Generic, Show)
instance FromJSON EventPerson where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "p" ""
instance ToJSON EventPerson where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "p" ""

data InsertableEventPerson = InsertableEventPerson { wpEmail :: Maybe Email
                                                   , wpDisplayName :: Maybe String
                                                   }
    deriving (Generic, Show)
instance ToJSON InsertableEventPerson where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "wp" ""

data EventDateTimeObject = EventDateTimeObject { edtDate :: Maybe EventDate
                                               , edtDateTime :: Maybe ApiDateTime
                                               , edtTimeZone :: Maybe ApiTimeZone
                                               }
    deriving (Generic, Show)
instance FromJSON EventDateTimeObject where
     parseJSON = genericParseJSON $ removePrefixLCFirstOpts "edt" ""
instance ToJSON EventDateTimeObject where
     toJSON = genericToJSON $ removePrefixLCFirstOpts "edt" ""

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


removePrefix prefix word = if (take len word) == prefix
                              then ntimes len tail word
                              else word
                                where ntimes n f = foldr (.) id (replicate n f)
                                      len = length prefix
lcFirst (a:as) = (toLower a) : as
removePrefixLCFirst prefix = lcFirst . (removePrefix prefix)

removePrefixLCFirstOpts flPrefix ctPrefix = defaultOptions { fieldLabelModifier = removePrefixLCFirst flPrefix
                                                       , constructorTagModifier = removePrefixLCFirst ctPrefix
                                                       , omitNothingFields = True
                                                       }





testParseJSONFromFile file = do json <- LT.readFile file
                                let jsonLBS = LT.encodeUtf8 json
                                return $ decode jsonLBS


transformFile func inputFile outPutFile = do result <- func inputFile
                                             writeFile outPutFile $ show result

testImportJSONFileOut func inputFile outPutFile = do result <- testParseJSONFromFile inputFile
                                                     writeFile outPutFile $ show $ func result
testImportJSON func inputFile = do result <- testParseJSONFromFile inputFile
                                   putStrLn $ show $ func result
testImportExportJSON func inputFile outPutFile = do result <- testParseJSONFromFile inputFile
                                                    L.writeFile outPutFile $ encode $ func result
