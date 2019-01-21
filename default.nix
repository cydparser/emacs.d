{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile path));

  fetchPinnedUrl = path: builtins.fetchurl (builtins.fromJSON (builtins.readFile path));

  hs = pkgs.haskellPackages;
in
rec {
  codex = hs.callCabal2nix "codex" (fetchPinnedGitHub nix/codex.json) {};

  dhall = pkgs.haskell.lib.dontCheck (hs.callPackage nix/dhall.nix { megaparsec = hs.megaparsec_7_0_0; inherit repline; });

  hies = callPackage nix/hies.nix { inherit fetchPinnedGitHub; };

  jdt-language-server = callPackage nix/jdt-language-server.nix { jre = pkgs.jre10_headless; inherit fetchPinnedUrl; };

  nix-linter = (import (fetchPinnedGitHub nix/nix-linter.json) { inherit pkgs; }).nix-linter;

  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix { inherit codex dhall hies jdt-language-server nix-linter mwebster-1913; };

  repline = hs.callPackage nix/repline.nix {};
}
