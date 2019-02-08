-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
--
-- Maintainer: Thomas Bach <hacking@babibo.de>
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB
    ( QueryResponse(..)
    , DataPointGroup(..)
    , WrappedQueryResponse(..)
    ) where

import           Data.Aeson      (FromJSON (..), withObject, (.:))
import           Data.Map.Strict (Map)
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics    (Generic)

newtype WrappedQueryResponse = WrappedQueryResponse { queries :: [QueryResponse] }
                             deriving (Eq, Generic, Show)

instance FromJSON WrappedQueryResponse

data QueryResponse = QueryResponse { sampleSize :: Integer
                                   , results    :: [DataPointGroup]
                                   }
                   deriving (Eq, Generic, Show)

instance FromJSON QueryResponse where
    parseJSON = withObject "QueryResponse" $ \v -> QueryResponse
                    <$> v .: "sample_size"
                    <*> v .: "results"

data DataPointGroup = DataPointGroup { name   :: Text
                                     , tags   :: Map Text [Text]
                                     , values :: [(UTCTime, Scientific)]
                                     }
                    deriving (Eq, Generic, Show)

instance FromJSON DataPointGroup
