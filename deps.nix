let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskellPackages;
in
[
  haskellPackages.callHackage "parsec" "3.1.14.0" {}
  haskellPackages.callHackage "haskeline" "0.7.5.0" {}
  haskellPackages.callHackage "hspec" "2.7.1" {}
]
