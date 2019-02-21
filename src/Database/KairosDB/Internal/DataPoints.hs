-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
-- Maintainer: Thomas Bach <hacking@babibo.de>
--
-- `data` and `newtype` declarations to represent communication with
-- KairosDB. This basically boils down to find a good representation
-- of the JSON instances. We try to be as close to KairosDB's
-- nomenclature as possible.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Database.KairosDB.Internal.DataPoints where

import Data.Aeson          (ToJSON (toJSON), object, (.=))
import Data.HashMap.Strict (HashMap)
import Data.Text           (Text)

import Database.KairosDB.Internal.Time

data DataPoints a = DataPoints
    { dataPointsName   :: Text
    , dataPointsType   :: Maybe Text
    , dataPointsTags   :: HashMap Text Text
    , dataPointsTtl    :: Maybe Int
    , dataPointsPoints :: [(KairosTimestamp, a)]
    }
    deriving (Eq,Show)

instance ToJSON a => ToJSON (DataPoints a) where
    toJSON DataPoints{..} = object
        (["name" .= dataPointsName, "datapoints" .= toJSON dataPointsPoints] <> type' <> tags <> ttl)
      where
        type' = maybe [] (\t -> ["type" .= t]) dataPointsType
        tags = if dataPointsTags == mempty
                   then mempty
                   else ["tags" .= dataPointsTags]
        ttl = maybe [] (\t -> ["ttl" .= t]) dataPointsTtl
