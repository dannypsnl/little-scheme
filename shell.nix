let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) mkShell;
  inherit (pkgs) haskellPackages;
  inherit (haskellPackages) cabal-install;

  ghc = haskellPackages.ghcWithPackages (pkgs: with pkgs; [base mtl]);
  dependencies = import ./deps.nix;
in
mkShell {
  buildInputs = [
    ghc
    cabal-install
    dependencies
  ];
}
