{ mkDerivation, base, hspec, hspec-discover, stdenv }:
mkDerivation {
  pname = "kairosdb-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec hspec-discover ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  description = "KairosDB client for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
