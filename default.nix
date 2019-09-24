{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  inherit (nixpkgs) haskellPackages;
in
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./little-scheme.nix {
  parsec = haskellPackages.callHackage "parsec" "3.1.14.0" {};
  haskeline = haskellPackages.callHackage "haskeline" "0.7.5.0" {};
  hspec = haskellPackages.callHackage "hspec" "2.7.1" {};
}
