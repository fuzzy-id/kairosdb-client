-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
--
-- Maintainer: Thomas Bach <hacking@babibo.de>
module Database.KairosDB
    ( module Database.KairosDB.Internal.Types
    , query
    ) where

import Network.HTTP.Req (JsonResponse, MonadHttp, Option, POST (POST),
                         ReqBodyJson (ReqBodyJson), Url, jsonResponse, req,
                         (/~))

import Database.KairosDB.Internal.Types

query :: MonadHttp m
      => Url scheme
      -> Option scheme
      -> QueryMetrics
      -> m (JsonResponse QueryResponse)
query scheme opts q = req POST url (ReqBodyJson q) jsonResponse opts
  where
    url = scheme /~ "api" /~ "v1" /~ "datapoints" /~ "query"

