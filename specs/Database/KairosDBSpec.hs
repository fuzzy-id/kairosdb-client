{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDBSpec (spec) where

import Data.HashMap.Strict (fromList)
import Test.Hspec          (Spec, beforeAll, describe, it, shouldReturn)

import Database.KairosDB

spec :: Spec
spec = beforeAll kairosDBSettingsFromEnv $ do
    describe "query with an empty request" $
        it "should return an empty response" $ \settings ->
            query settings emptyReq `shouldReturn` Success emptyResp
    describe "query with non-existent metric" $ do
        let expected = WrappedQueryResponse
                [QueryResponse 0 [DataPointGroup "non-existent" mempty mempty mempty]]
                :: WrappedQueryResponse ()
        it "should return an empty datapoint" $ \settings ->
            query settings nonExistentReq `shouldReturn` Success expected
        it "should have tags appended" $ \settings ->
            query settings nonExistentReqWithTags `shouldReturn` Success expected
    describe "adding datapoints" $ do
        it "should work on on empty list" $ \settings ->
            datapoints settings ([] :: [DataPoints ()]) `shouldReturn` SuccessNoContent
        it "should work on empty datapoints" $ \settings ->
            datapoints settings [emptyDataPoints] `shouldReturn` SuccessNoContent

emptyReq :: QueryMetrics
emptyReq = QueryMetrics (Left (Relative 1 Years)) Nothing Nothing []

emptyResp :: WrappedQueryResponse ()
emptyResp = WrappedQueryResponse []

nonExistentReq :: QueryMetrics
nonExistentReq = emptyReq { queryMetricsMetrics = [Metric "non-existent" mempty Nothing []] }

nonExistentReqWithTags :: QueryMetrics
nonExistentReqWithTags =
    emptyReq { queryMetricsMetrics = [Metric "non-existent" (fromList [("foo", ["baz", "bar"])]) Nothing []]}

-- TODO We _have_ to provide tags. Reflect this in the type-system.
emptyDataPoints :: DataPoints ()
emptyDataPoints = DataPoints "some-name" Nothing (fromList [("foo","bar")]) Nothing mempty
