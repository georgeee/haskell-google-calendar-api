{-# LANGUAGE DeriveGeneric, TemplateHaskell   #-}
module Network.Google.Calendar.CalendarList.Entities where

import qualified Data.Text                       as T
import           Network.Google.Calendar.Entities
import           Network.Google.ApiIO.Generators (genJSONInstances, genRecJSONInstances, genTagJSONInstances)
import           GHC.Generics

data Resource = Resource { cleEtag :: ETag
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
$(genRecJSONInstances "cle" ''Resource)

data InsertableResource = InsertableResource { icleId :: CalendarId
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
$(genRecJSONInstances "icle" ''InsertableResource)
defaultInsertableResource calendarId = InsertableResource { icleId  = calendarId
                                                          , icleSummaryOverride = Nothing
                                                          , icleColorId = Nothing
                                                          , icleBackgroundColor = Nothing
                                                          , icleForegroundColor = Nothing
                                                          , icleHidden = Nothing
                                                          , icleSelected = Nothing
                                                          , icleDefaultReminders = Nothing
                                                          , icleNotificationSettings = Nothing
                                                          }

data UpdatableResource = UpdatableResource { ucleSummaryOverride :: Maybe T.Text
                                           , ucleColorId :: Maybe CalendarColorId
                                           , ucleBackgroundColor :: Maybe Color
                                           , ucleForegroundColor :: Maybe Color
                                           , ucleHidden :: Maybe Bool
                                           , ucleSelected :: Maybe Bool
                                           , ucleDefaultReminders :: Maybe [Reminder]
                                           , ucleNotificationSettings :: Maybe CalendarNotificationSettings
                                           }
    deriving (Generic, Show)
$(genRecJSONInstances "ucle" ''UpdatableResource)
