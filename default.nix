{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile path));

  fetchPinnedUrl = path: builtins.fetchurl (builtins.fromJSON (builtins.readFile path));

  hs = pkgs.haskellPackages.extend (_super: self: {
    haskell-src-exts = self.callPackage nix/haskell-src-exts.nix {};

    repline = hs.callPackage nix/repline.nix {};
  });

  skipTests = pkgs.haskell.lib.dontCheck;
in
rec {
  codex = hs.callCabal2nix "codex" (fetchPinnedGitHub nix/codex.json) {};

  dhall = skipTests (hs.callPackage nix/dhall.nix { megaparsec = hs.megaparsec_7_0_0; });

  emacs = pkgs.emacsWithPackages (epkgs: with epkgs.melpaStablePackages; [
    epkgs.pdf-tools
  ]);

  hasktags = hs.callPackage nix/hasktags.nix {};

  hies = callPackage nix/hies.nix { inherit fetchPinnedGitHub; };

  hlint = hs.callPackage nix/hlint.nix {};

  jdt-language-server = callPackage nix/jdt-language-server.nix { jre = pkgs.jre10_headless; inherit fetchPinnedUrl; };

  nix-linter = (import (fetchPinnedGitHub nix/nix-linter.json) { inherit pkgs; }).nix-linter;

  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix { inherit
    codex
    dhall
    emacs
    hasktags
    hies
    hlint
    jdt-language-server
    nix-linter
    mwebster-1913
    ;
  };
}
