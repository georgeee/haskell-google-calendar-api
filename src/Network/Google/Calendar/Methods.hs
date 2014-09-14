{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Google.Calendar.Methods where

import           GHC.Generics

import           Network.Google.Calendar.Entities         as E
import           Network.Google.Calendar.Methods.Internal
import           Network.Google.Calendar.Api

class DefaultParams a where
    defaultParams :: a

-- Method calendar#calendarList

data CalendarListListResponse = CalendarListListResponse { cllrEtag :: ETag
                                                         , cllrNextPageToken :: Maybe PageToken
                                                         , cllrNextSyncToken :: Maybe SyncToken
                                                         , cllrItems :: [CalendarListEntry]
                                                         }
                                deriving (Generic, Show)
$(generatePagableResponseInstances "cllr" ''CalendarListListResponse)

data CalendarListListRequestParams = CalendarListListRequestParams { cllrpMaxResults :: Maybe Int
                                                                   , cllrpMinAccessRole :: Maybe CalendarAccessRole
                                                                   , cllrpPageToken :: Maybe PageToken
                                                                   , cllrpShowDeleted :: Maybe Bool
                                                                   , cllrpShowHidden :: Maybe Bool
                                                                   , cllrpSyncToken :: Maybe SyncToken
                                                                   }
                                deriving (Generic, Show)
$(generatePagableRequestInstances "users/me/calendarList" "cllrp" ''CalendarListListRequestParams)
instance DefaultParams CalendarListListRequestParams where
    defaultParams = CalendarListListRequestParams Nothing Nothing Nothing Nothing Nothing Nothing

calendarListList :: MethodTag CalendarListListRequestParams CalendarListListResponse
calendarListList = readOnlyScopeMethod
