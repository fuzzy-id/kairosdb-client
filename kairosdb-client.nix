{ mkDerivation, aeson, base, bytestring, connection, data-default
, hspec, hspec-discover, http-client, http-client-tls, req
, scientific, stdenv, text, time, unordered-containers
}:
mkDerivation {
  pname = "kairosdb-client";
  version = "0.4.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring connection data-default http-client
    http-client-tls req scientific text time unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring hspec hspec-discover req time
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  description = "KairosDB client for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
