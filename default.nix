{ stdenv, fetchFromGitHub, fetchurl, unzip
, ctags
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
  inherit (haskellPackages) apply-refact hasktags codex;

  flycheck-yaml-ruby = ruby;

  hies = let
    src = fetchFromGitHub {
      owner = "domenkozar";
      repo = "hie-nix";
      rev = "a270d8db4551f988437ac5db779a3cf614c4af68";
      sha256 = "0hilxgmh5aaxg37cbdwixwnnripvjqxbvi8cjzqrk7rpfafv352q";
    };
    in (import src {}).hies;

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
  name = "emacs.d";

  buildInputs = [
    apply-refact
    # codex
    flycheck-yaml-ruby
    hasktags
    hies
    hlint
    hunspellDict
    shellcheck
    xmllint
    mwebster-1913
  ];

  postInstall = ''
    wrapProgram ${emacs}/bin/emacs \
      --suffix-each PATH : "${shellcheck}/bin" \
      --set DICPATH "${hunspellDict}/share/hunspell" \
      --set STARDICT_DATA_DIR ${mwebster-1913}
  '';
}
