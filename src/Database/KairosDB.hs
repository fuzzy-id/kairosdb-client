-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
--
-- Maintainer: Thomas Bach <hacking@babibo.de>
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Database.KairosDB
    ( module Database.KairosDB.Internal.Config
    , module Database.KairosDB.Internal.DataPoints
    , module Database.KairosDB.Internal.QueryMetrics
    , module Database.KairosDB.Internal.Time
    , ApiResponse(..)
    , query
    , datapoints
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson             (FromJSON, ToJSON, eitherDecode)
import Network.HTTP.Req       (MonadHttp, POST (POST),
                               ReqBodyJson (ReqBodyJson), Scheme (..), Url,
                               http, https, lbsResponse, port, req,
                               responseBody, responseStatusCode, runReq, (/~))

import Database.KairosDB.Internal.Config
import Database.KairosDB.Internal.DataPoints
import Database.KairosDB.Internal.QueryMetrics
import Database.KairosDB.Internal.Time

query :: (FromJSON a, MonadIO m)
      => KairosDBSettings
      -> QueryMetrics
      -> m (ApiResponse (WrappedQueryResponse a))
query s q = runReq httpConfig request
  where
    request = mkRequest (\base -> base /~ "datapoints" /~ "query") s q
    httpConfig = kairosDBSettingsHttpConfig s

datapoints :: (MonadIO m, ToJSON a)
           => KairosDBSettings
           -> [DataPoints a]
           -> m (ApiResponse ())
datapoints s points = runReq httpConfig request
  where
    httpConfig = kairosDBSettingsHttpConfig s
    request = mkRequest (\base -> base /~ "datapoints") s points

mkRequest :: (MonadHttp m, ToJSON body, FromJSON a)
        => (forall scheme. Url scheme -> Url scheme)
        -> KairosDBSettings
        -> body
        -> m (ApiResponse a)
mkRequest method KairosDBSettings{..} body =
    do resp <- case kairosDBSettingsScheme of
                   Http  -> r . path $ (http kairosDBSettingsHost)
                   Https -> r . path $ (https kairosDBSettingsHost)
       case responseStatusCode resp of
         200 -> let rBody = responseBody resp
                    json = eitherDecode rBody
                in either error (return . Success) json
         204 -> return SuccessNoContent
         _ -> undefined
  where
    path x = method (x /~ "api" /~ "v1")
    r url = req POST url (ReqBodyJson body) lbsResponse (port kairosDBSettingsPort)

data ApiResponse a = Success a
                   | SuccessNoContent
                   deriving (Eq, Show)
