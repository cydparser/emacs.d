{ mkDerivation, base, bytestring, containers, directory, filepath
, HUnit, json, microlens-platform, optparse-applicative, stdenv
, utf8-string
}:
mkDerivation {
  pname = "hasktags";
  version = "0.71.2";
  sha256 = "2c40fdcb834e8ac1d588daf0e8ace5b58c776cbde2d56892a75106ff324e53e8";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath json microlens-platform
    utf8-string
  ];
  executableHaskellDepends = [
    base containers directory filepath optparse-applicative
  ];
  testHaskellDepends = [
    base bytestring directory filepath HUnit json microlens-platform
    utf8-string
  ];
  homepage = "http://github.com/MarcWeber/hasktags";
  description = "Produces ctags \"tags\" and etags \"TAGS\" files for Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
