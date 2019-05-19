let
  readJSON = path: builtins.fromJSON (builtins.readFile path);

  pkgs = import (builtins.fetchTarball (readJSON nix/nixpkgs.json)) {};

  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub (readJSON path);

  fetchPinnedUrl = path: builtins.fetchurl (readJSON path);

  hs = pkgs.haskellPackages;

  skipTests = pkgs.haskell.lib.dontCheck;
in
rec {
  codex = hs.callCabal2nix "codex" (fetchPinnedGitHub nix/codex.json) {};

  dhall = skipTests (hs.callPackage nix/dhall.nix {});

  emacs = pkgs.emacsWithPackages (epkgs: with epkgs.melpaStablePackages; [
    epkgs.pdf-tools
  ]);


  jdt-language-server = callPackage nix/jdt-language-server.nix { jdk = pkgs.jdk; inherit fetchPinnedUrl; };


  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix { inherit
    codex
    dhall
    emacs
    jdt-language-server
    mwebster-1913
    ;
  };
}
