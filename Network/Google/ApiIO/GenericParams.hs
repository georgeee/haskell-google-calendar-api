{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, TypeOperators,
             FlexibleInstances, FlexibleContexts, OverlappingInstances     #-}
module Network.Google.ApiIO.GenericParams where

import Control.Applicative ((<*>), (<$>), (<|>), pure)
import GHC.Generics
import Data.DList (DList, toList, empty)
import Data.Monoid (mappend)
import Network.Google.ApiIO.Common
import qualified Data.Text                       as T


class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString Int where
    toString = show

instance ToString Bool where
    toString = show

instance ToString T.Text where
    toString = T.unpack

instance (ToString s) => ToString (Maybe s) where
    toString (Just v) = toString v
    toString Nothing = ""

type Pair = (String, String)

genericParams :: (Generic a, GEntity (Rep a)) => Options -> a -> [(String, String)]
genericParams opts = extractParams opts . from

class GEntity f where
    extractParams :: Options -> f a -> [(String, String)]

instance (GEntity f) => GEntity (M1 i c f) where
    extractParams opts = extractParams opts . unM1

instance (ToString a) => ToString (K1 i a p) where
    toString = toString . unK1

instance GEntity U1 where
    extractParams _ _ = []

data Options = Options { fieldLabelModifier :: String -> String
                       , omitNothingFields :: Bool
                       }
defaultOptions = Options id True 
removePrefixLCFirstOpts prefix = defaultOptions { fieldLabelModifier = removePrefixLCFirst prefix }

instance (RecordToPairs f) => GEntity (C1 c f) where
    extractParams opts = toList . recordToPairs opts. unM1

class RecordToPairs f where
    recordToPairs :: Options -> f a -> DList Pair

instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
    recordToPairs opts (a :*: b) = recordToPairs opts a `mappend`
                                   recordToPairs opts b

instance (Selector s, ToString c) => RecordToPairs (S1 s (K1 i c)) where
    recordToPairs = fieldToPair

instance (Selector s, ToString c) => RecordToPairs (S1 s (K1 i (Maybe c))) where
    recordToPairs opts (M1 (K1 Nothing)) | omitNothingFields opts = empty
    recordToPairs opts m1 = fieldToPair opts m1

fieldToPair opts m1 = pure ( fieldLabelModifier opts $ selName m1
                           , toString ( unM1 m1  )
                           )
