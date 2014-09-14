{-# LANGUAGE TypeSynonymInstances, FlexibleInstances     #-}
module Network.Google.Calendar.Common where
import Data.Char (toLower)
import Control.Monad

removePrefix prefix word = if (take len word) == prefix
                              then ntimes len tail word
                              else word
                                where ntimes n f = foldr (.) id (replicate n f)
                                      len = length prefix
lcFirst (a:as) = (toLower a) : as
removePrefixLCFirst prefix = lcFirst . (removePrefix prefix)

class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString Int where
    toString = show

instance ToString Bool where
    toString = show

instance (ToString s) => ToString (Maybe s) where
    toString (Just v) = toString v
    toString Nothing = ""


replacePair (n, v) = ((n, v) :) . filter (\(x, _) -> x /= n)

replacePairs :: (Eq a) => [(a, b)] -> [(a, b)] -> [(a, b)]
replacePairs = foldr replacePair

concatM :: (Monad m) => [m [a]] -> m [a]
concatM = foldr (liftM2 (++)) (return [])

