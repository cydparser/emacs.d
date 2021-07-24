{ lib, makeFontsConf, makeWrapper, runCommandNoCC
, ag
, apply-refact
, codex
, delta
, emacs
, emacsPackages
, espeak
, gnutls
, haskellPackages
, hlint
, hunspell
, hunspellDicts
, libxml2
, mwebster-1913
, nixpkgs-lint
, openssl
, python3
, ripgrep
, rnix-lsp
, ruby
, sdcv
, shellcheck
, wordnet
, z3
}:
let
  inherit (emacsPackages) cask;
  inherit (haskellPackages) dhall hasktags;

  flycheck-yaml = ruby;

  xmllint = libxml2.bin;
in
  runCommandNoCC "mx" { name = "emacs+"; buildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin

    makeWrapper ${emacs}/bin/emacs $out/bin/mx \
      --suffix PATH : ${lib.makeBinPath [
        ag
        apply-refact
        cask
        codex
        delta
        dhall
        emacs
        espeak
        flycheck-yaml
        hasktags
        hlint
        hunspell
        mwebster-1913
        nixpkgs-lint
        python3
        ripgrep
        rnix-lsp
        sdcv
        shellcheck
        wordnet
        xmllint
        z3
      ]} \
      --set CODEX_DISABLE_WORKSPACE true \
      --set DICPATH "${hunspellDicts.en-us}/share/hunspell" \
      --set STARDICT_DATA_DIR "${mwebster-1913}" \
      --set WORDLIST "$XDG_CONFIG_HOME/ispell/words"
  ''
