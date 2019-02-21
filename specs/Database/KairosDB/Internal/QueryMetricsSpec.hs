{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB.Internal.QueryMetricsSpec (spec) where

import Data.Aeson           (eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict  (fromList)
import Data.Time            (UTCTime (UTCTime), fromGregorian,
                             secondsToDiffTime)
import Test.Hspec           (Spec, describe, it, shouldBe)

import Database.KairosDB.Internal.QueryMetrics

spec :: Spec
spec = do
    describe "Decoding JSON" $ do
        it "should work on minimal example" $
            eitherDecode minimal `shouldBe`
                Right emptyResp
        it "should work on example with points" $
            eitherDecode withPoints `shouldBe`
                Right (WrappedQueryResponse
                           [QueryResponse
                                2
                                [DataPointGroup
                                     "foo"
                                     (fromList [("baz",["bar"])])
                                     [GroupByType "number"]
                                     [ (KairosTimestamp (UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)), 1.0)
                                     , (KairosTimestamp (UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 1)), 2.0)
                                     ]
                                ]
                           ]
                       :: WrappedQueryResponse Float)
    describe "Encoding JSON" $ do
        it "should work on query" $
            encode (QueryMetrics (Left (Relative 5 Minutes)) Nothing Nothing [])
                `shouldBe` "{\"metrics\":[],\"start_relative\":{\"value\":5,\"unit\":\"minutes\"}}"


minimal :: ByteString
minimal = "{\"queries\":[{\"sample_size\":0,\"results\":[{\"name\":\"minimal\",\"tags\":{},\"values\":[]}]}]}"

withPoints :: ByteString
withPoints = "{\"queries\":[{\"sample_size\":2,\"results\":[{\"name\":\"foo\",\"group_by\":[{\"name\":\"type\",\"type\":\"number\"}],\"tags\":{\"baz\":[\"bar\"]},\"values\":[[0,1.0],[1000,2.0]]}]}]}"

emptyResp :: WrappedQueryResponse ()
emptyResp = WrappedQueryResponse [QueryResponse 0 [DataPointGroup "minimal" mempty [] []]]
