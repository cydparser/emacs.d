{ fetchPinnedGitHub, makeWrapper, runCommandNoCC }:
let
  src = fetchPinnedGitHub ./hie-nix.json;

  pkgs = import (import "${src}/fetch-nixpkgs.nix") {};

  hie80Pkgs = (import "${src}/ghc-8.0.nix" { inherit pkgs; }).override {
    overrides = _self: _super: { Cabal = null; };
  };

  hie80 = pkgs.haskell.lib.justStaticExecutables hie80Pkgs.haskell-ide-engine;

  orig = import src { inherit pkgs; };
in
  runCommandNoCC "hies" { buildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin
    ln -s ${hie80}/bin/hie $out/bin/hie-8.0
    makeWrapper ${orig.hie84}/bin/hie-wrapper $out/bin/hie-wrapper \
      --prefix PATH : $out/bin:${orig.hies}/bin
  ''
