let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskellPackages;
  parsec = haskellPackages.callHackage "parsec" "3.1.14.0" {};
in
[
  parsec
]
