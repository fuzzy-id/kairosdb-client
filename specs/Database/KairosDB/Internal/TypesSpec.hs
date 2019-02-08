{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB.Internal.TypesSpec (spec) where

import Data.Aeson           (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Test.Hspec           (Spec, describe, it, shouldBe)

import Database.KairosDB.Internal.Types

spec :: Spec
spec = describe "Parsing JSON" $ do
    describe "minimal result" $ do
        it "should properly parse" $
            eitherDecode minimal `shouldBe`
                Right (WrappedQueryResponse [QueryResponse 0 [DataPointGroup "minimal" mempty []]])


minimal :: ByteString
minimal = "{\"queries\":[{\"sample_size\":0,\"results\":[{\"name\":\"minimal\",\"tags\":{},\"values\":[]}]}]}"
