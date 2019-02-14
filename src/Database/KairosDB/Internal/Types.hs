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
{-# LANGUAGE RecordWildCards   #-}
module Database.KairosDB.Internal.Types
    ( -- * Query Responses from KairosDB
      QueryResponse(..)
    , DataPointGroup(..)
    , GroupBy(..)
    , KairosTimestamp(..)
    , WrappedQueryResponse(..)
    -- * Constructing Queries
    , QueryMetrics(..)
    , Metric(..)
    , Aggregator(..)
    , Relative(..)
    , KairosTimeunit(..)
    ) where

import Data.Aeson.Types    (FromJSON (..), ToJSON (..), Value (Number, String),
                            object, typeMismatch, withObject, (.:), (.:?), (.=))
import Data.HashMap.Strict (HashMap, toList)
import Data.Maybe          (fromMaybe)
import Data.Scientific     (Scientific)
import Data.Text           (Text)
import Data.Time           (UTCTime (UTCTime), addUTCTime, diffUTCTime,
                            fromGregorian, secondsToDiffTime)
import GHC.Generics        (Generic)

-- Query responses

-- | Responses from KairosDB are wrapped in an additional `{
-- "queries": [ ... results ... ] }`. You should not observe this type
-- in any high-level functions.
newtype WrappedQueryResponse =
    WrappedQueryResponse { queries :: [QueryResponse] }
    deriving (Eq, Generic, Show)

instance FromJSON WrappedQueryResponse

data QueryResponse = QueryResponse
    { queryResponseSampleSize :: Int              -- ^ The size of the result
    , queryResponseResults    :: [DataPointGroup]
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
    , tags    :: HashMap Text [Text]
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
        fromMilliSecs x = x / realToFrac (1000 :: Int)
    parseJSON v = typeMismatch "Milliseconds" v

instance ToJSON KairosTimestamp where
    toJSON = toJSON . toMilliSecs . (epoch `diffUTCTime`) . getUTCTime
      where
        toMilliSecs = (* realToFrac (1000 :: Int))

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

-- | Haskell representation of the several ways to group data points.
data GroupBy = GroupByType Text
             deriving (Eq, Show)

instance FromJSON GroupBy where
    parseJSON = withObject "GroupBy" $ \v -> do
        name <- v .: "name"
        case name of
          "type" -> GroupByType <$> v .: "type"
          _      -> fail ("Cannot handle group by: " <> name)

-- Building queries

-- TODO Add timezone
data QueryMetrics = QueryMetrics
    { queryMetricsStart     :: Either Relative KairosTimestamp
    , queryMetricsEnd       :: Maybe (Either Relative KairosTimestamp)
    , queryMetricsCacheTime :: Maybe Int
    , queryMetricsMetrics   :: [Metric]
    }
    deriving (Eq, Show)

instance ToJSON QueryMetrics where
    toJSON QueryMetrics {..} = object
        (start <> end <> cacheTime <> [ "metrics" .= queryMetricsMetrics ])
      where
        start = case queryMetricsStart of
                  Left rel   -> ["start_relative" .= rel]
                  Right abs' -> ["start_absolute" .= abs']
        end = case queryMetricsEnd of
                Nothing           -> []
                Just (Left rel)   -> ["end_relative" .= rel]
                Just (Right abs') -> ["end_absolute" .= abs']
        cacheTime = maybe [] (\t -> ["cache_time" .= t]) queryMetricsCacheTime

data Metric = Metric
    { metricName        :: Text
    , metricTags        :: HashMap Text [Text]
    , metricLimit       :: Maybe Int
    , metricAggregators :: [Aggregator]
    }
    deriving (Eq, Show)

instance ToJSON Metric where
    toJSON Metric {..} = object
        (["name" .= metricName] <> tags <> limit <> aggs)
      where
        tags = if metricTags == mempty
                  then []
                  else ["tags" .= metricTags]
        limit = maybe [] (\l -> ["limit" .= l]) metricLimit
        aggs = if metricAggregators == mempty
                  then []
                  else ["aggregators" .= metricAggregators]
data Aggregator = Aggregator
    { aggregatorName       :: Text
    , aggregatorSampling   :: Maybe Relative
    , aggregatorAdditional :: HashMap Text Value
    }
    deriving (Eq, Show)

instance ToJSON Aggregator where
    toJSON Aggregator {..} = object
        ([ "name" .= aggregatorName] <> sampling <> additional)
      where
        sampling = maybe [] (\s -> ["sampling" .= s]) aggregatorSampling
        additional = toList aggregatorAdditional

data Relative = Relative { relativeValue :: Int
                         , relativeUnit  :: KairosTimeunit
                         }
              deriving (Eq, Show)

instance ToJSON Relative where
    toJSON (Relative v u) = object [ "value" .= v, "unit" .= u ]

data KairosTimeunit =
    Milliseconds | Seconds | Minutes | Hours | Days | Weeks | Months | Years
  deriving (Eq, Show)

instance ToJSON KairosTimeunit where
    toJSON Milliseconds = String "milliseconds"
    toJSON Seconds      = String "seconds"
    toJSON Minutes      = String "minutes"
    toJSON Hours        = String "hours"
    toJSON Days         = String "days"
    toJSON Weeks        = String "weeks"
    toJSON Months       = String "months"
    toJSON Years        = String "years"
