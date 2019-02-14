{ mkDerivation, aeson, base, bytestring, connection, containers
, data-default, hspec, hspec-discover, http-client, http-client-tls
, req, scientific, stdenv, text, time
}:
mkDerivation {
  pname = "kairosdb-client";
  version = "0.2.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring connection containers data-default
    http-client http-client-tls req scientific text time
  ];
  testHaskellDepends = [
    aeson base bytestring containers hspec hspec-discover req time
  ];
  testToolDepends = [ hspec-discover ];
  description = "KairosDB client for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
