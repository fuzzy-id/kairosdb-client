-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
-- Maintainer: Thomas Bach <hacking@babibo.de>
--
-- `data` and `newtype` declarations to represent communication with
-- KairosDB. This basically boils down to find a good representation
-- of the JSON instances. We try to be as close to KairosDB's
-- nomenclature as possible.
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB.Internal.Types
    ( -- * Query Responses from KairosDB
      QueryResponse(..)
    , DataPointGroup(..)
    , GroupBy(..)
    , KairosTimestamp(..)
    , WrappedQueryResponse(..)
    ) where

import Data.Aeson       (FromJSON (..), Value (Number), withObject,
                         withScientific, (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Map.Strict  (Map)
import Data.Maybe       (fromMaybe)
import Data.Scientific  (Scientific)
import Data.Text        (Text)
import Data.Time        (UTCTime (UTCTime), addUTCTime, fromGregorian,
                         secondsToDiffTime)
import GHC.Generics     (Generic)

-- | Responses from KairosDB are wrapped in an additional `{
-- "queries": [ ... results ... ] }`. You should not observe this type
-- in any high-level functions.
newtype WrappedQueryResponse =
    WrappedQueryResponse { queries :: [QueryResponse] }
    deriving (Eq, Generic, Show)

instance FromJSON WrappedQueryResponse

data QueryResponse = QueryResponse
    { sampleSize :: Int              -- ^ The size of the result
    , results    :: [DataPointGroup]
    }
    deriving (Eq, Show)

instance FromJSON QueryResponse where
    parseJSON = withObject "QueryResponse" $ \v -> QueryResponse
                    <$> v .: "sample_size"
                    <*> v .: "results"

-- | A group of data points where a data point is a tuple of timestamp
-- and value.
data DataPointGroup = DataPointGroup
    { name    :: Text
    , tags    :: Map Text [Text]
    , groupBy :: [GroupBy]
    , values  :: [(KairosTimestamp, Scientific)]
    }
    deriving (Eq, Show)

instance FromJSON DataPointGroup where
    parseJSON = withObject "DataPointGroup" $ \v -> DataPointGroup
                    <$> v .: "name"
                    <*> v .: "tags"
                    <*> (fromMaybe mempty <$> (v .:? "group_by"))
                    <*> v .: "values"

-- | KairosDB represents time as milliseconds since epoch. This is a
-- simple wrapper around 'UTCTime' to ensure proper JSON
-- (de-)serialization.
newtype KairosTimestamp = KairosTimestamp { getUTCTime :: UTCTime }
                        deriving (Eq, Show)

instance FromJSON KairosTimestamp where
    parseJSON v@(Number _) =
        KairosTimestamp . (`addUTCTime` epoch) . fromMilliSecs <$> parseJSON v
      where
        epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
        fromMilliSecs x = x / realToFrac 1000
    parseJSON v = typeMismatch "Milliseconds" v

-- | Haskell representation of the several ways to group data points.
data GroupBy = GroupByType Text
             deriving (Eq, Show)

instance FromJSON GroupBy where
    parseJSON = withObject "GroupBy" $ \v -> do
        name <- v .: "name"
        case name of
          "type" -> GroupByType <$> v .: "type"
          _      -> fail ("Cannot handle group by: " <> name)
