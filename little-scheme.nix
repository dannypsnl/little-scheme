{ mkDerivation, base, mtl, dependencies, stdenv }:
mkDerivation {
  pname = "little-scheme";
  version = "0.1.0";
  src = stdenv.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl ] ++ dependencies;
  license = stdenv.lib.licenses.mit;
}
