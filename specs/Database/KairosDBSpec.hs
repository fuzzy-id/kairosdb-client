{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDBSpec (spec) where

import Data.Map.Strict (fromList)
import Test.Hspec      (Spec, beforeAll, describe, it, shouldReturn)

import Database.KairosDB

spec :: Spec
spec = beforeAll kairosDBSettingsFromEnv $ do
    describe "query with an empty request" $
        it "should return an empty response" $ \settings -> do
            query settings emptyReq `shouldReturn` emptyResp
    describe "query with non-existent metric" $ do
        let expected = WrappedQueryResponse
                [QueryResponse 0 [DataPointGroup "non-existent" mempty mempty mempty]]
        it "should return an empty datapoint" $ \settings -> do
            query settings nonExistentReq `shouldReturn` expected
        it "should have tags appended" $ \settings -> do
            query settings nonExistentReqWithTags `shouldReturn` expected

emptyReq :: QueryMetrics
emptyReq = QueryMetrics (Left (Relative 1 Years)) Nothing Nothing []

emptyResp :: WrappedQueryResponse
emptyResp = WrappedQueryResponse []

nonExistentReq :: QueryMetrics
nonExistentReq = emptyReq { queryMetricsMetrics = [Metric "non-existent" mempty Nothing []] }

nonExistentReqWithTags :: QueryMetrics
nonExistentReqWithTags =
    emptyReq { queryMetricsMetrics = [Metric "non-existent" (fromList [("foo", ["baz", "bar"])]) Nothing []]}
