module Network.Google.ApiIO.Common where

import           Network.HTTP.Types              as H (urlEncode)
import qualified Data.ByteString.Char8           as B8
import           Data.Char (toLower)
import           Control.Monad
import           Data.Monoid (mappend, mempty)
import           Network.HTTP.QueryString        as Q

removePrefix prefix word = if (take len word) == prefix
                              then ntimes len tail word
                              else word
                                where ntimes n f = foldr (.) id (replicate n f)
                                      len = length prefix
lcFirst (a:as) = (toLower a) : as
removePrefixLCFirst prefix = lcFirst . (removePrefix prefix)

concatM :: (Monad m) => [m [a]] -> m [a]
concatM = foldr (liftM2 (++)) (return [])

urlEncode :: String -> String
urlEncode = B8.unpack . H.urlEncode False . B8.pack

modifyQS :: B8.ByteString -> [(String, String)] -> B8.ByteString
modifyQS orig ps = let orig' = maybe mempty id $ Q.parseQuery orig                                                        
                       ps'   = Q.queryString $ map (\(x, y) -> (B8.pack x, B8.pack y)) ps
                   in Q.toString $ orig' `mappend` ps'

replacePair :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
replacePair (n, v) = ((n, v) :) . filter (\(x, _) -> x /= n)

replacePairs :: (Eq a) => [(a, b)] -> [(a, b)] -> [(a, b)]
replacePairs = foldr replacePair
