{ lib, makeFontsConf, makeWrapper, runCommandNoCC
, emacs
, emacsPackages
, gnutls # XXX
, haskellPackages
, hunspell
, hunspellDicts
, libxml2
, mwebster-1913
, nixpkgs-lint
, openssl # XXX
, python3
, sdcv
, silver-searcher
, wordnet
, z3
}:
let
  inherit (emacsPackages) cask;

  xmllint = libxml2.bin;
in
  runCommandNoCC "mx" { name = "emacs+"; buildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin

    makeWrapper ${emacs}/bin/emacs $out/bin/mx \
      --suffix PATH : ${lib.makeBinPath [
        emacs
        mwebster-1913
        nixpkgs-lint
        xmllint
      ]} \
      --set DICPATH "${hunspellDicts.en-us}/share/hunspell" \
      --set STARDICT_DATA_DIR "${mwebster-1913}" \
      --set WORDLIST "$XDG_CONFIG_HOME/ispell/words"
  ''
