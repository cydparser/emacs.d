{ fetchFromGitHub, makeWrapper, runCommandNoCC }:
let
  src = fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "a7ef4c4ceef1dbf46aabff68a4b9bd86d41d0886";
    sha256 = "1hx449l001jc6ijn9zxx30zr1xr2nnrv7qmhpsqwj8wp6s4zyxw8";
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
