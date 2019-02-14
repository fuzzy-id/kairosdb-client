-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
--
-- Maintainer: Thomas Bach <hacking@babibo.de>
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Database.KairosDB
    ( module Database.KairosDB.Internal.Types
    , module Database.KairosDB.Internal.Config
    , query
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types       (FromJSON, ToJSON)
import Network.HTTP.Req       (JsonResponse, MonadHttp, POST (POST),
                               ReqBodyJson (ReqBodyJson), Scheme (..), Url,
                               http, https, jsonResponse, port, req,
                               responseBody, runReq, (/~))

import Database.KairosDB.Internal.Config
import Database.KairosDB.Internal.Types

query :: (FromJSON a, MonadIO m)
      => KairosDBSettings
      -> QueryMetrics
      -> m (WrappedQueryResponse a)
query s q = responseBody <$> runReq httpConfig request
  where
    request = mkQuery (\base -> base /~ "datapoints" /~ "query") s q
    httpConfig = kairosDBSettingsHttpConfig s

mkQuery :: (MonadHttp m, ToJSON q, FromJSON r)
        => (forall scheme. Url scheme -> Url scheme)
        -> KairosDBSettings
        -> q
        -> m (JsonResponse r)
mkQuery method KairosDBSettings{..} q = case kairosDBSettingsScheme of
    Http  -> r . path $ (http kairosDBSettingsHost)
    Https -> r . path $ (https kairosDBSettingsHost)
  where
    path x = method (x /~ "api" /~ "v1")
    r url = req POST url (ReqBodyJson q) jsonResponse (port kairosDBSettingsPort)
