let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskellPackages;
  parsec = haskellPackages.callHackage "parsec" "3.1.14.0" {};
  haskeline = haskellPackages.callHackage "haskeline" "0.7.5.0" {};
in
[
  parsec
  haskeline
]
