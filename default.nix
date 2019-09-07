let
  pkgs = import <nixpkgs> { };

  dependencies = import ./deps.nix;
in
  pkgs.haskellPackages.callPackage ./little-scheme.nix { dependencies=dependencies; }
