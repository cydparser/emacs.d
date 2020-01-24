{ stdenv, makeFontsConf, makeWrapper, runCommandNoCC
, ag
, all-hies
, codex
, emacs
, emacs-all-the-icons-fonts
, emacsPackages
, espeak
, gnutls
, haskellPackages
, hasklig
, hlint
, hunspell
, hunspellDicts
, jdt-language-server
, libxml2
, mwebster-1913
, nixpkgs-lint
, openssl
, ripgrep
, ruby
, sdcv
, shellcheck
, source-code-pro
, symbola
, wordnet
, z3
}:
let
  inherit (emacsPackages) cask;
  inherit (haskellPackages) apply-refact dhall hasktags;

  flycheck-yaml = ruby;

  fontsConf = makeFontsConf {
    fontDirectories = [
      emacs-all-the-icons-fonts
      hasklig
      source-code-pro
      symbola
    ];
  };

  hies = all-hies.bios.selection { selector = p: { inherit (p) ghc865; }; };

  xmllint = libxml2.bin;
in
  runCommandNoCC "mx" { name = "emacs+"; buildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin
    makeWrapper ${emacs}/bin/emacs $out/bin/mx \
      --suffix PATH : ${stdenv.lib.makeBinPath [
        ag
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
        jdt-language-server
        mwebster-1913
        nixpkgs-lint
        ripgrep
        sdcv
        shellcheck
        wordnet
        xmllint
        z3
      ]} \
      --set CODEX_DISABLE_WORKSPACE true \
      --set DICPATH "${hunspellDicts.en-us}/share/hunspell" \
      --set FONTCONFIG_FILE "${fontsConf}" \
      --set JDT_LSP "${jdt-language-server}" \
      --set STARDICT_DATA_DIR "${mwebster-1913}" \
      --set WORDLIST "$XDG_CONFIG_HOME/ispell/words"
  ''
