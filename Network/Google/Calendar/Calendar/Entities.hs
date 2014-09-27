{-# LANGUAGE DeriveGeneric, TemplateHaskell   #-}
module Network.Google.Calendar.Calendar.Entities where

import qualified Data.Text                       as T
import           Network.Google.Calendar.Entities
import           Network.Google.ApiIO.Generators (genJSONInstances, genRecJSONInstances, genTagJSONInstances)
import           GHC.Generics
import           Network.Google.ApiIO.Entities

data Calendar = Calendar { cEtag :: ETag
                         , cId :: CalendarId
                         , cSummary :: T.Text
                         , cDescription :: Maybe T.Text
                         , cLocation :: Maybe Location
                         , cTimeZone :: Maybe ApiTimeZone
                         }
                    deriving (Generic, Show)
$(genRecJSONInstances "c" ''Calendar)

data InsertableCalendar = InsertableCalendar { icSummary :: T.Text }
                    deriving (Generic, Show)
$(genRecJSONInstances "ic" ''InsertableCalendar)

data UpdatableCalendar = UpdatableCalendar { ucSummary :: Maybe T.Text
                                           , ucDescription :: Maybe T.Text
                                           , ucLocation :: Maybe Location
                                           , ucTimeZone :: Maybe ApiTimeZone
                                           }
                    deriving (Generic, Show)
$(genRecJSONInstances "uc" ''UpdatableCalendar)
