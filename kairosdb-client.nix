{ mkDerivation, aeson, base, bytestring, containers, hspec
, hspec-discover, scientific, stdenv, text, time
}:
mkDerivation {
  pname = "kairosdb-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers scientific text time
  ];
  testHaskellDepends = [
    aeson base bytestring containers hspec hspec-discover time
  ];
  testToolDepends = [ hspec-discover ];
  description = "KairosDB client for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
