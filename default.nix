{ stdenv, fetchFromGitHub, fetchurl, makeWrapper, unzip
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
  inherit (haskellPackages) apply-refact codex hasktags;

  flycheck-yaml-ruby = ruby;

  hies = let
    src = fetchFromGitHub {
      owner = "domenkozar";
      repo = "hie-nix";
      rev = "a270d8db4551f988437ac5db779a3cf614c4af68";
      sha256 = "0hilxgmh5aaxg37cbdwixwnnripvjqxbvi8cjzqrk7rpfafv352q";
    };
    in (import "${src}/default.nix" {}).hies;

  hnix-lsp = let
    src = fetchFromGitHub {
      owner = "domenkozar";
      repo = "hnix-lsp";
      rev = "acc1d29c2d061c3354f57faf6455dbc9767a5644";
      sha256 = "0516bh6d7scpv4xq0d91bl6h4b20xlygx543gqy6ldj5yddml8n6";
    };
    in (import "${src}/stack2nix.nix" {}).hnix-lsp;

  hunspellDict = hunspellDicts.en-us;

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
        # codex
        espeak
        flycheck-yaml-ruby
        hasktags
        hies
        hlint
        hnix-lsp
        hunspell
        mwebster-1913
        nixpkgs-lint
        sdcv
        shellcheck
        wordnet
        xmllint
      ]} \
      --set DICPATH "${hunspellDict}/share/hunspell" \
      --set STARDICT_DATA_DIR ${mwebster-1913}
  '';
}
