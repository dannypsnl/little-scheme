{ mkDerivation, base, stdenv }:
let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskellPackages;
  mtl = haskellPackages.callHackage "mtl" "2.2.2" {};
  parsec = haskellPackages.callHackage "parsec" "3.1.14.0" {};
in
mkDerivation {
  pname = "little-scheme";
  version = "0.1.0";
  src = stdenv.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl parsec ];
  license = stdenv.lib.licenses.mit;
}
