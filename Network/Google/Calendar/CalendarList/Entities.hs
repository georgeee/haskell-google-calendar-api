{-# LANGUAGE DeriveGeneric, TemplateHaskell   #-}
module Network.Google.Calendar.CalendarList.Entities where

import           Control.Monad
import qualified Data.Text                       as T
import           Data.ByteString.Lazy            as L
import           Network.Google.Calendar.Entities
import           Network.Google.ApiIO.Generators (genJSONInstances, genRecJSONInstances, genTagJSONInstances, genRecToJSON)
import           GHC.Generics
import           Data.Aeson

data CalendarLERaw = CalendarLERaw { cleRawEtag :: ETag
                                   , cleRawId :: CalendarId
                                   , cleRawSummary :: T.Text
                                   , cleRawDescription :: Maybe T.Text
                                   , cleRawLocation :: Maybe Location
                                   , cleRawTimeZone :: Maybe ApiTimeZone
                                   , cleRawSummaryOverride :: Maybe T.Text
                                   , cleRawColorId :: Maybe CalendarColorId
                                   , cleRawBackgroundColor :: Maybe Color
                                   , cleRawForegroundColor :: Maybe Color
                                   , cleRawHidden :: Maybe Bool
                                   , cleRawSelected :: Maybe Bool
                                   , cleRawAccessRole :: CalendarAccessRole
                                   , cleRawDefaultReminders :: Maybe [Reminder]
                                   , cleRawNotificationSettings :: Maybe CalendarNotificationSettings
                                   , cleRawPrimary :: Maybe Bool
                                   , cleRawDeleted :: Maybe Bool
                                   }
    deriving (Generic, Show)
$(genRecJSONInstances "cleRaw" ''CalendarLERaw)

data CalendarLE = CalendarLE { cleEtag :: ETag
                             , cleId :: CalendarId
                             , cleSummary :: T.Text
                             , cleDescription :: Maybe T.Text
                             , cleLocation :: Maybe Location
                             , cleTimeZone :: Maybe ApiTimeZone
                             , cleSummaryOverride :: Maybe T.Text
                             , cleColorId :: Maybe CalendarColorId
                             , cleBackgroundColor :: Maybe Color
                             , cleForegroundColor :: Maybe Color
                             , cleHidden :: Bool
                             , cleSelected :: Bool
                             , cleAccessRole :: CalendarAccessRole
                             , cleDefaultReminders :: [Reminder]
                             , cleNotificationSettings :: [CalendarNotification]
                             , clePrimary :: Bool
                             , cleDeleted :: Bool
                             }
    deriving (Generic, Show)

instance FromJSON CalendarLE where
    parseJSON x = parseJSON x >>= return . extractRawCalendar

extractRawCalendar x = CalendarLE { cleEtag = cleRawEtag x
                                  , cleId = cleRawId x
                                  , cleSummary = cleRawSummary x
                                  , cleDescription = cleRawDescription x
                                  , cleLocation = cleRawLocation x
                                  , cleTimeZone = cleRawTimeZone x
                                  , cleSummaryOverride = cleRawSummaryOverride x
                                  , cleColorId = cleRawColorId x
                                  , cleBackgroundColor = cleRawBackgroundColor x
                                  , cleForegroundColor = cleRawForegroundColor x
                                  , cleHidden = optBool $ cleRawHidden x
                                  , cleSelected = optBool $ cleRawSelected x
                                  , cleAccessRole = cleRawAccessRole x
                                  , cleDefaultReminders = optArray $ cleRawDefaultReminders x
                                  , cleNotificationSettings = optNotifications $ cleRawNotificationSettings x
                                  , clePrimary = optBool $ cleRawPrimary x
                                  , cleDeleted = optBool $ cleRawDeleted x
                                  }
    where optBool = maybe False id
          optArray = maybe [] id
          optNotifications = maybe [] (\(CalendarNotificationSettings ns) -> ns)
$(genRecToJSON "cle" ''CalendarLE)

data InsertableCalendarLE = InsertableCalendarLE { icleId :: CalendarId
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
$(genRecJSONInstances "icle" ''InsertableCalendarLE)
defaultInsertableCalendarLE calendarId = InsertableCalendarLE { icleId  = calendarId
                                                          , icleSummaryOverride = Nothing
                                                          , icleColorId = Nothing
                                                          , icleBackgroundColor = Nothing
                                                          , icleForegroundColor = Nothing
                                                          , icleHidden = Nothing
                                                          , icleSelected = Nothing
                                                          , icleDefaultReminders = Nothing
                                                          , icleNotificationSettings = Nothing
                                                          }

data UpdatableCalendarLE = UpdatableCalendarLE { ucleSummaryOverride :: Maybe T.Text
                                           , ucleColorId :: Maybe CalendarColorId
                                           , ucleBackgroundColor :: Maybe Color
                                           , ucleForegroundColor :: Maybe Color
                                           , ucleHidden :: Maybe Bool
                                           , ucleSelected :: Maybe Bool
                                           , ucleDefaultReminders :: Maybe [Reminder]
                                           , ucleNotificationSettings :: Maybe CalendarNotificationSettings
                                           }
    deriving (Generic, Show)
$(genRecJSONInstances "ucle" ''UpdatableCalendarLE)
