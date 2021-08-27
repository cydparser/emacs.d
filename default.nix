let
  readJSON = path: builtins.fromJSON (builtins.readFile path);

  fetchPinnedUrl = path: builtins.fetchurl (readJSON path);

  pkgs = import (builtins.fetchTarball (readJSON nix/nixpkgs.json)) {};

  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub ((readJSON path) // { fetchSubmodules = true; });

  rnix-lsp = import (builtins.fetchGit { inherit (readJSON ./nix/rnix-lsp.json) url rev; });

  inherit (pkgs.haskellPackages) apply-refact hlint;

in
rec {
  codex = callPackage nix/codex.nix { inherit fetchPinnedGitHub; };

  delta = pkgs.gitAndTools.delta;

  emacs =
    let
      emacsWithXwidets = pkgs.emacs.override {
        webkitgtk = pkgs.webkitgtk;
        withXwidgets = true;
      };
    in (pkgs.emacsPackagesFor emacsWithXwidets).emacsWithPackages (epkgs: with epkgs.melpaStablePackages; [
      pdf-tools
    ]);

  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix {
    inherit
      apply-refact
      codex
      delta
      emacs
      hlint
      rnix-lsp
      mwebster-1913
    ;
    ripgrep = pkgs.ripgrep.override { withPCRE2 = true; };
  };
}
