-- |
-- Copyright:  2019 Thomas Bach
-- License  :  BSD3
-- Maintainer: Thomas Bach <hacking@babibo.de>
--
-- `data` and `newtype` declarations to represent communication with
-- KairosDB. This basically boils down to find a good representation
-- of the JSON instances. We try to be as close to KairosDB's
-- nomenclature as possible.
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Database.KairosDB.Internal.Config
    ( KairosDBSettings (..)
    , kairosDBSettingsFromEnv
    ) where

import Data.Char                (toLower)
import Data.Default             (def)
import Data.Text                (Text, pack)
import Network.Connection       (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS  (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Req         (HttpConfig (httpConfigAltManager), Scheme (..))
import System.Environment.Blank (getEnvDefault)

data KairosDBSettings = KairosDBSettings
    { kairosDBSettingsHttpConfig :: HttpConfig
    , kairosDBSettingsScheme     :: Scheme
    , kairosDBSettingsHost       :: Text
    , kairosDBSettingsPort       :: Int
    }

kairosDBSettingsFromEnv :: IO KairosDBSettings
kairosDBSettingsFromEnv = do
    envScheme <- getEnvDefault "KAIROSDB_SCHEME" "http"
    let scheme = case map toLower envScheme of
                   "http"  -> Http
                   "https" -> Https
                   _ -> error "KAIROSDB_SCHEME has to be one of http or https."
    manager <- case scheme of
                   Http -> newManager defaultManagerSettings
                   Https -> do
                       validateCert <- read <$> getEnvDefault "KAIROSDB_VALIDATE_CERT" "True"
                       let settings = mkManagerSettings tlsSettings Nothing
                           tlsSettings = TLSSettingsSimple validateCert False False
                       newTlsManagerWith settings
    host <- pack <$> getEnvDefault "KAIROSDB_HOST" "localhost"
    port <- read <$> getEnvDefault "KAIROSDB_PORT" "8080"
    return (KairosDBSettings (def { httpConfigAltManager = Just manager }) scheme host port)

