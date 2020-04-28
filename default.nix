let
  readJSON = path: builtins.fromJSON (builtins.readFile path);

  pkgs = import (builtins.fetchTarball (readJSON nix/nixpkgs.json)) {};

  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub ((readJSON path) // { fetchSubmodules = true; });

  fetchPinnedUrl = path: builtins.fetchurl (readJSON path);

  hs = pkgs.haskell.packages.ghc8101.extend (self: super: {
    ghc-exactprint = skipTests (super.callPackage ./nix/ghc-exactprint.nix {});
  });

  inherit (hs) apply-refact;

  allowNewer = pkgs.haskell.lib.doJailbreak;

  skipTests = pkgs.haskell.lib.dontCheck;
in
rec {
  codex = callPackage nix/codex.nix { inherit fetchPinnedGitHub; };

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
      emacs
      jdt-language-server
      mwebster-1913
    ;
    ripgrep = pkgs.ripgrep.override { withPCRE2 = true; };
  };
}
