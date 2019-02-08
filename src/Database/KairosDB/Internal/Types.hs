-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
--
-- Maintainer: Thomas Bach <hacking@babibo.de>
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB.Internal.Types
    ( QueryResponse(..)
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

newtype WrappedQueryResponse = WrappedQueryResponse { queries :: [QueryResponse] }
                             deriving (Eq, Generic, Show)

instance FromJSON WrappedQueryResponse

data QueryResponse = QueryResponse { sampleSize :: Integer
                                   , results    :: [DataPointGroup]
                                   }
                   deriving (Eq, Show)

instance FromJSON QueryResponse where
    parseJSON = withObject "QueryResponse" $ \v -> QueryResponse
                    <$> v .: "sample_size"
                    <*> v .: "results"

data DataPointGroup = DataPointGroup { name   :: Text
                                     , tags   :: Map Text [Text]
                                     , groupBy :: [GroupBy]
                                     , values :: [(KairosTimestamp, Scientific)]
                                     }
                    deriving (Eq, Show)

instance FromJSON DataPointGroup where
    parseJSON = withObject "DataPointGroup" $ \v -> DataPointGroup
                    <$> v .: "name"
                    <*> v .: "tags"
                    <*> (fromMaybe mempty <$> (v .:? "group_by"))
                    <*> v .: "values"

newtype KairosTimestamp = KairosTimestamp { getUTCTime :: UTCTime }
                        deriving (Eq, Show)

instance FromJSON KairosTimestamp where
    parseJSON v@(Number _) =
        KairosTimestamp . (`addUTCTime` utc0) . fromMilliSecs <$> parseJSON v
      where
        utc0 = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
        fromMilliSecs x = x / realToFrac 1000
    parseJSON v = typeMismatch "Milliseconds" v

data GroupBy = GroupByType Text
             deriving (Eq, Show)

instance FromJSON GroupBy where
    parseJSON = withObject "GroupBy" $ \v -> do
        name <- v .: "name"
        case name of
          "type" -> GroupByType <$> v .: "type"
          _      -> fail ("Cannot handle group by: " <> name)
