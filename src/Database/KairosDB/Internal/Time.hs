-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
-- Maintainer: Thomas Bach <hacking@babibo.de>
--
module Database.KairosDB.Internal.Time
    ( KairosTimestamp(..)
    ) where

import Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON), Value (Number))
import Data.Aeson.Types (typeMismatch)
import Data.Time        (UTCTime (UTCTime), addUTCTime, diffUTCTime,
                         fromGregorian, secondsToDiffTime)

-- | KairosDB represents time as milliseconds since epoch. This is a
-- simple wrapper around 'UTCTime' to ensure proper JSON
-- (de-)serialization.
newtype KairosTimestamp = KairosTimestamp { getUTCTime :: UTCTime }
                        deriving (Eq, Show)

instance FromJSON KairosTimestamp where
    parseJSON v@(Number _) =
        KairosTimestamp . (`addUTCTime` epoch) . fromMilliSecs <$> parseJSON v
      where
        fromMilliSecs x = x / realToFrac (1000 :: Int)
    parseJSON v = typeMismatch "Milliseconds" v

instance ToJSON KairosTimestamp where
    toJSON = toJSON . toMilliSecs . (epoch `diffUTCTime`) . getUTCTime
      where
        toMilliSecs = (* realToFrac (1000 :: Int))

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
