{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB.Internal.TypesSpec (spec) where

import Data.Aeson           (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict      (fromList)
import Data.Time            (UTCTime (UTCTime), fromGregorian,
                             secondsToDiffTime)
import Test.Hspec           (Spec, describe, it, shouldBe)

import Database.KairosDB.Internal.Types

spec :: Spec
spec = describe "Parsing responses from JSON" $ do
    it "should parse minimal example" $
        eitherDecode minimal `shouldBe`
            Right (WrappedQueryResponse [QueryResponse 0 [DataPointGroup "minimal" mempty [] []]])
    it "should parse example with points" $
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
                       ])

minimal :: ByteString
minimal = "{\"queries\":[{\"sample_size\":0,\"results\":[{\"name\":\"minimal\",\"tags\":{},\"values\":[]}]}]}"

withPoints :: ByteString
withPoints = "{\"queries\":[{\"sample_size\":2,\"results\":[{\"name\":\"foo\",\"group_by\":[{\"name\":\"type\",\"type\":\"number\"}],\"tags\":{\"baz\":[\"bar\"]},\"values\":[[0,1.0],[1000,2.0]]}]}]}"
