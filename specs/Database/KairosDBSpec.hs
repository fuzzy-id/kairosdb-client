{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDBSpec (spec) where

import Control.Concurrent  (threadDelay)
import Data.HashMap.Strict (fromList, singleton)
import Data.Time           (getCurrentTime)
import Test.Hspec          (Spec, beforeAll, describe, it, runIO, shouldBe,
                            shouldReturn, shouldSatisfy)

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
    describe "adding and querying points" $
        do now <- KairosTimestamp <$> runIO getCurrentTime
           let q = QueryMetrics (Right nowMinusOneSec) Nothing Nothing [Metric "some-name" mempty Nothing mempty]
               nowMinusOneSec = addToKairosTimestamp (toEnum $ -10^(12 :: Int)) now
           it "should return empty response first" $ \settings ->
               query settings q
                   `shouldReturn`
                       ([QueryResponse 0 [DataPointGroup "some-name" mempty mempty mempty]] :: [QueryResponse Int])
           it "should add actual values" $ \settings ->
               datapoints settings [emptyDataPoints {dataPointsPoints = [(now, 1 :: Int)]}]
                   `shouldReturn` ()
           it "should then be able to query these values" $ \settings -> do
               threadDelay 500000
               resp <- query settings q
               resp `shouldSatisfy` ((== 1) . length)
               let [QueryResponse num dpGroup] = resp
               num `shouldBe` 1
               dpGroup `shouldSatisfy` ((== 1) . length)
               let [DataPointGroup name tags groupBy vals] = dpGroup
               name `shouldBe` "some-name"
               tags `shouldBe` singleton "foo" ["bar"]
               groupBy `shouldBe` [GroupByType "number"]
               vals `shouldSatisfy` ((== 1) . length)
               let [(ts, val)] = vals
               val `shouldBe` (1 :: Int)
               ts `shouldSatisfy` (> nowMinusOneSec)
               ts `shouldSatisfy` (< now)
               -- [QueryResponse 1 [DataPointGroup "some-name" (singleton "foo" ["bar"]) [GroupByType "number"] [(now, 1 :: Int)]]]



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
