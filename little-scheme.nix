{ mkDerivation, base, filepath, haskeline, hspec, mtl, parsec
, stdenv
}:
mkDerivation {
  pname = "little-scheme";
  version = "0.1.0.0";
  src = stdenv.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base filepath mtl parsec ];
  executableHaskellDepends = [ base haskeline mtl parsec ];
  testHaskellDepends = [ base hspec mtl ];
  license = stdenv.lib.licenses.mit;
}
