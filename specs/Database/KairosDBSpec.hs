{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDBSpec (spec) where

import Data.HashMap.Strict (fromList)
import Test.Hspec          (Spec, beforeAll, describe, it, shouldReturn)

import Database.KairosDB

spec :: Spec
spec = beforeAll kairosDBSettingsFromEnv $ do
    describe "query with an empty request" $
        it "should return an empty response" $ \settings ->
            query settings emptyReq `shouldReturn` ([] :: [QueryResponse ()])
    describe "query with non-existent metric" $ do
        let expected =
                [QueryResponse 0 [DataPointGroup "non-existent" mempty mempty mempty]]
                :: [QueryResponse ()]
        it "should return an empty datapoint" $ \settings ->
            query settings nonExistentReq `shouldReturn` expected
        it "should have tags appended" $ \settings ->
            query settings nonExistentReqWithTags `shouldReturn` expected
    describe "adding datapoints" $ do
        it "should work on on empty list" $ \settings ->
            datapoints settings ([] :: [DataPoints ()]) `shouldReturn` ()
        it "should work on empty datapoints" $ \settings ->
            datapoints settings [emptyDataPoints] `shouldReturn` ()

emptyReq :: QueryMetrics
emptyReq = QueryMetrics (Left (Relative 1 Years)) Nothing Nothing []

nonExistentReq :: QueryMetrics
nonExistentReq = emptyReq { queryMetricsMetrics = [Metric "non-existent" mempty Nothing []] }

nonExistentReqWithTags :: QueryMetrics
nonExistentReqWithTags =
    emptyReq { queryMetricsMetrics = [Metric "non-existent" (fromList [("foo", ["baz", "bar"])]) Nothing []]}

-- TODO We _have_ to provide tags. Reflect this in the type-system.
emptyDataPoints :: DataPoints ()
emptyDataPoints = DataPoints "some-name" Nothing (fromList [("foo","bar")]) Nothing mempty
