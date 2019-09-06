let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) mkShell;
  inherit (pkgs) haskellPackages;

  little-scheme = import ./default.nix;
  ghc = haskellPackages.ghcWithPackages (pkgs: with pkgs; [base mtl]);
in
mkShell {
  buildInputs = [ little-scheme ghc haskellPackages.cabal-install ];
}
