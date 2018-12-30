{ stdenv, fetchFromGitHub, fetchurl, makeWrapper, pkgs, runCommandNoCC, unzip
, emacs
, espeak
, haskellPackages
, hlint
, hunspell
, hunspellDicts
, libxml2
, nixpkgs-lint
, ruby
, sdcv
, shellcheck
, wordnet
}:
let
  inherit (haskellPackages) apply-refact hasktags;

  codex = let
    src = fetchFromGitHub {
      owner = "aloiscochard";
      repo = "codex";
      rev = "48b2a4b94132b537943bcd966c3922cbf67a7409";
      sha256 = "1mwsz824rqsphy68frqdrpkdmvlqg3zfn7wb565r8621jbbn9iws";
    };
    in haskellPackages.callCabal2nix "codex" src {};

  flycheck-yaml = ruby;

  hies = let
    src = fetchFromGitHub {
      owner = "domenkozar";
      repo = "hie-nix";
      rev = "a270d8db4551f988437ac5db779a3cf614c4af68";
      sha256 = "0hilxgmh5aaxg37cbdwixwnnripvjqxbvi8cjzqrk7rpfafv352q";
    };

    ppkgs = import (import "${src}/fetch-nixpkgs.nix") {};

    hie80Pkgs = (import "${src}/ghc-8.0.nix" { pkgs = ppkgs; }).override {
      overrides = self: super: { Cabal = null; };
    };

    hie80 = pkgs.haskell.lib.justStaticExecutables hie80Pkgs.haskell-ide-engine;

    orig = import src { pkgs = ppkgs; };

    in runCommandNoCC "hies" { buildInputs = [ makeWrapper ]; } ''
         mkdir -p $out/bin
         ln -s ${hie80}/bin/hie $out/bin/hie-8.0
         makeWrapper ${orig.hie84}/bin/hie-wrapper $out/bin/hie-wrapper \
           --prefix PATH : $out/bin:${orig.hies}/bin
       '';

  hunspellDict = hunspellDicts.en-us;

  nix-linter = let
    src = fetchFromGitHub {
      owner = "Synthetica9";
      repo = "nix-linter";
      rev = "31666b492bfacb58149a2efdcfbeec73b01b44a2";
      sha256 = "02ssc6f6rvww9n6n4ydcnygk75mhqzcx3n80p8cw9kb0zccdqcn4";
    };
    in (import src { inherit pkgs; }).nix-linter;

  mwebster-1913 = stdenv.mkDerivation {
    name = "merriam-webster-1913";

    sourceRoot = ".";

    src = fetchurl {
      url = "https://s3.amazonaws.com/jsomers/dictionary.zip";
      sha256 = "09y673q5v46ps72pnm1jz0jx2bcyfbsmzw3f2x9y9s99k3yf79ps";
    };

    nativeBuildInputs = [ unzip ];

    installPhase = ''
      mkdir -p "$out/dic"
      tar -C "$out/dic" -xf dictionary/stardict-dictd-web1913-2.4.2.tar.bz2
    '';
  };

  xmllint = libxml2.bin;

in stdenv.mkDerivation {
  name = "mx";

  buildInputs = [ makeWrapper ];

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    makeWrapper ${emacs}/bin/emacs $out/bin/mx \
      --suffix PATH : ${stdenv.lib.makeBinPath [
        apply-refact
        codex
        espeak
        flycheck-yaml
        hasktags
        hies
        hlint
        hunspell
        mwebster-1913
        nix-linter
        nixpkgs-lint
        sdcv
        shellcheck
        wordnet
        xmllint
      ]} \
      --set CODEX_DISABLE_WORKSPACE true \
      --set DICPATH "${hunspellDict}/share/hunspell" \
      --set STARDICT_DATA_DIR "${mwebster-1913}" \
      --set WORDLIST "$XDG_CONFIG_HOME/ispell/words"
  '';
}
