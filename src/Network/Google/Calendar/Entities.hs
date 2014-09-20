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

import           Network.Google.ApiIO.Entities
import           Network.Google.ApiIO.Common (removePrefixLCFirst)
import           Network.Google.ApiIO.Generators (genJSONInstances, genRecJSONInstances, genTagJSONInstances)
import           Network.Google.ApiIO.GenericParams (ToString(..))
-- @TODO clean input types (E.g. Event), Maybe Bool => Bool, Maybe [] -> [], remove ToJSON instance (check that no use)


type Location = String
type ETag = String
type CalendarId = String
type ApiTimeZone = String
type CalendarColorId = String
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

data CalendarNotificationMethod = CNMEmail | CNMSms
    deriving (Generic, Show)
$(genTagJSONInstances "CNM" ''CalendarNotificationMethod)

data CalendarNotificationType = CNTEventCreation | CNTEventChange | CNTEventCancellation | CNTEventResponse | CNTAgenda
   deriving (Generic, Show)
$(genTagJSONInstances "CNT" ''CalendarNotificationType)

data CalendarNotification = CalendarNotification {
    cnMethod :: CalendarNotificationMethod,
    cnType :: CalendarNotificationType
}
    deriving (Generic, Show)
$(genRecJSONInstances "cn" ''CalendarNotification)

data CalendarNotificationSettings = CalendarNotificationSettings {
    cnsNotifications :: [CalendarNotification]
}
    deriving (Generic, Show)
$(genRecJSONInstances "cns" ''CalendarNotificationSettings)

data CRMethod = CRMEmail | CRMSms | CRMPopup
                deriving (Generic, Show)
$(genTagJSONInstances "CRM" ''CRMethod)
                  
data Reminder = Reminder { crMethod :: CRMethod
                         , crMinutes :: Int
                         }
                        deriving (Generic, Show)
$(genRecJSONInstances "cr" ''Reminder)


data CalendarAccessRole = FreeBusyReader | Reader | Writer | Owner
                        deriving (Generic, Show)
$(genTagJSONInstances "" ''CalendarAccessRole)
instance ToString CalendarAccessRole where
    toString = removePrefixLCFirst "" . show






