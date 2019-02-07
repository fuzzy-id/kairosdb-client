{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "kairosdb-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  description = "KairosDB client for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
