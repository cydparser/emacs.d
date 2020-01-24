{ mkDerivation, base, bytestring, directory, extra, filepath
, ghc-lib-parser, stdenv, tasty, tasty-hunit, uniplate
}:
mkDerivation {
  pname = "ghc-lib-parser-ex";
  version = "8.8.2";
  sha256 = "92a86ea0f57a8cfa37e3a4d3614c90a2e80a6384f4022d07f8c20a2529b7a198";
  libraryHaskellDepends = [
    base bytestring ghc-lib-parser uniplate
  ];
  testHaskellDepends = [
    base directory extra filepath ghc-lib-parser tasty tasty-hunit
  ];
  homepage = "https://github.com/shayne-fletcher/ghc-lib-parser-ex#readme";
  description = "Algorithms on GHC parse trees";
  license = stdenv.lib.licenses.bsd3;
}
