{ pkgs, fetchPinnedGitHub }:
let
  hs = pkgs.haskellPackages;

  ghc-lib-parser-ex = hs.callPackage ./ghc-lib-parser-ex.nix {};

  src = fetchPinnedGitHub ./hlint.json;
in
hs.callCabal2nix "hlint" src { inherit ghc-lib-parser-ex; }
