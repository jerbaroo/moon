{ mkDerivation, base, containers, pretty-simple, stdenv, text }:
mkDerivation {
  pname = "moon";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers pretty-simple text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/jerbaroo/moon";
  description = "Infrastructure as code in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
