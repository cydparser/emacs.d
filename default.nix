let
  readJSON = path: builtins.fromJSON (builtins.readFile path);

  fetchPinnedUrl = path: builtins.fetchurl (readJSON path);

  pkgs = import (builtins.fetchTarball (readJSON nix/nixpkgs.json)) {};

  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub ((readJSON path) // { fetchSubmodules = true; });

  haskell-nix =
    let
      hn = import (fetchPinnedGitHub ./nix/haskell-nix.json) {};
        # import (builtins.fetchTarball (readJSON ./nix/haskell-nix.json)) {};
    in (import hn.sources.nixpkgs-2003 hn.nixpkgsArgs).haskell-nix;

  rnix-lsp = import (builtins.fetchTarball (readJSON ./nix/rnix-lsp.json));

  inherit (pkgs.haskellPackages) apply-refact hlint;

in
rec {
  codex = callPackage nix/codex.nix { inherit fetchPinnedGitHub; };

  delta = pkgs.gitAndTools.delta;

  emacs =
    let epkgs = pkgs.emacsPackages.overrideScope' (self: super: {
      emacs = super.emacs.override {
        inherit (pkgs) webkitgtk;
        withXwidgets = true;
      };
    });
    in epkgs.emacsWithPackages (p: with p.melpaStablePackages; [ p.pdf-tools ]);

  jdt-language-server = callPackage nix/jdt-language-server.nix { jdk = pkgs.jdk; inherit fetchPinnedUrl; };

  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix {
    inherit
      apply-refact
      codex
      delta
      emacs
      hlint
      jdt-language-server
      rnix-lsp
      mwebster-1913
    ;
    ripgrep = pkgs.ripgrep.override { withPCRE2 = true; };
  };
}
