{ fetchFromGitHub, makeWrapper, runCommandNoCC }:
let
  src = fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "a270d8db4551f988437ac5db779a3cf614c4af68";
    sha256 = "0hilxgmh5aaxg37cbdwixwnnripvjqxbvi8cjzqrk7rpfafv352q";
  };

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
