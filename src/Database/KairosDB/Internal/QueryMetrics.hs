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
module Database.KairosDB.Internal.QueryMetrics
    ( -- * Query Responses from KairosDB
      QueryResponse(..)
    , DataPointGroup(..)
    , GroupBy(..)
    , WrappedQueryResponse(..)
    -- * Constructing Queries
    , QueryMetrics(..)
    , Metric(..)
    , Aggregator(..)
    , Relative(..)
    , KairosTimeunit(..)
    ) where

import Data.Aeson.Types    (FromJSON (..), ToJSON (..), Value (String), object,
                            withObject, (.:), (.:?), (.=))
import Data.HashMap.Strict (HashMap, toList)
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import GHC.Generics        (Generic)

import Database.KairosDB.Internal.Time

-- Query responses

-- | Responses from KairosDB are wrapped in an additional `{
-- "queries": [ ... results ... ] }`. You should not observe this type
-- in any high-level functions.
newtype WrappedQueryResponse a =
    WrappedQueryResponse { queries :: [QueryResponse a] }
    deriving (Eq, Generic, Show)

instance FromJSON a => FromJSON (WrappedQueryResponse a)

data QueryResponse a = QueryResponse
    { queryResponseSampleSize :: Int              -- ^ The size of the result
    , queryResponseResults    :: [DataPointGroup a]
    }
    deriving (Eq, Show)

instance FromJSON a => FromJSON (QueryResponse a) where
    parseJSON = withObject "QueryResponse" $ \v -> QueryResponse
                    <$> v .: "sample_size"
                    <*> v .: "results"

-- | A group of data points where a data point is a tuple of timestamp
-- and value.
data DataPointGroup a = DataPointGroup
    { dataPointGroupName    :: Text
    , dataPointGroupTags    :: HashMap Text [Text]
    , dataPointGroupGroupBy :: [GroupBy]
    , dataPointGroupValues  :: [(KairosTimestamp, a)]
    }
    deriving (Eq, Show)

instance FromJSON a => FromJSON (DataPointGroup a) where
    parseJSON = withObject "DataPointGroup" $ \v -> DataPointGroup
                    <$> v .: "name"
                    <*> v .: "tags"
                    <*> (fromMaybe mempty <$> (v .:? "group_by"))
                    <*> v .: "values"

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
