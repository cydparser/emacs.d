{ stdenv, makeWrapper, runCommandNoCC
, codex
, dhall
, emacs
, emacsPackages
, espeak
, haskellPackages
, hies
, hlint
, hunspell
, hunspellDicts
, jdt-language-server
, libxml2
, mwebster-1913
, nix-linter
, nixpkgs-lint
, ruby
, sdcv
, shellcheck
, wordnet
}:
let
  inherit (emacsPackages) cask;
  inherit (haskellPackages) apply-refact hasktags;

  flycheck-yaml = ruby;

  xmllint = libxml2.bin;
in
  runCommandNoCC "mx" { buildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin
    makeWrapper ${emacs}/bin/emacs $out/bin/mx \
      --suffix PATH : ${stdenv.lib.makeBinPath [
        apply-refact
        cask
        codex
        dhall
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
      --set DICPATH "${hunspellDicts.en-us}/share/hunspell" \
      --set JDT_LSP "${jdt-language-server}" \
      --set STARDICT_DATA_DIR "${mwebster-1913}" \
      --set WORDLIST "$XDG_CONFIG_HOME/ispell/words"
  ''
