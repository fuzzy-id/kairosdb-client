{-# LANGUAGE OverloadedStrings #-}
module Database.KairosDB.Internal.TimeSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Time  (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)

import Database.KairosDB.Internal.Time

spec :: Spec
spec =
  describe "KairosTimestamp" $ do
    describe "JSON encoding" $ do
      it "should encode epoch to 0" $
          encode epoch `shouldBe` "0"
      it "should encode epoch plus one sec to 1000" $
          encode epochPlusOneSec `shouldBe` "1000"
    describe "JSON decoding" $ do
      it "should decode 0 to epoch" $
          eitherDecode "0" `shouldBe` Right epoch
      it "should decode 1000 to epoch plus one sec" $
          eitherDecode "1000" `shouldBe` Right epochPlusOneSec
    describe "adding time" $ do
      it "should work with positive NominalDiffTime" $
        addToKairosTimestamp (toEnum $ 10^(12 :: Int)) epoch
          `shouldBe` epochPlusOneSec
      it "should work with negative NominalDiffTime" $
        addToKairosTimestamp (toEnum $ -10^(12 :: Int)) epochPlusOneSec
          `shouldBe` epoch

epoch :: KairosTimestamp
epoch = KairosTimestamp (UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0))

epochPlusOneSec :: KairosTimestamp
epochPlusOneSec = KairosTimestamp (UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 1))
