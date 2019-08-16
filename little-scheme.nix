{ mkDerivation, base, stdenv }:
let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskellPackages;
  parsec = haskellPackages.callHackage "parsec" "3.1.13.0" {};
in
mkDerivation {
  pname = "little-scheme";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base parsec ];
  license = stdenv.lib.licenses.mit;
}
