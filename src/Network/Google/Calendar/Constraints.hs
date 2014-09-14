module Network.Google.Calendar.Constraints (calendarAPIUrlBase, composeGoogleKey) where

import           Network.OAuth.OAuth2
import           Data.ByteString                 as B
import qualified Data.ByteString.Char8           as B8

calendarAPIUrlBase = "https://www.googleapis.com/calendar/v3/"

composeGoogleKey :: ByteString -> ByteString -> OAuth2
composeGoogleKey clientId clientSecret = OAuth2 { oauthClientId = clientId
                                                , oauthClientSecret = clientSecret
                                                , oauthCallback = Just $ B8.pack "urn:ietf:wg:oauth:2.0:oob"
                                                , oauthOAuthorizeEndpoint = B8.pack "https://accounts.google.com/o/oauth2/auth"
                                                , oauthAccessTokenEndpoint = B8.pack "https://accounts.google.com/o/oauth2/token"
                                                }


