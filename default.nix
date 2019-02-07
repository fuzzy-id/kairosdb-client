{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  kairosdbClient = import ./kairosdb-client.nix;
in
haskellPackages.callPackage kairosdbClient {}
