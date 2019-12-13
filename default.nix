let
  readJSON = path: builtins.fromJSON (builtins.readFile path);

  pkgs = import (builtins.fetchTarball (readJSON nix/nixpkgs.json)) {};

  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub ((readJSON path) // { fetchSubmodules = true; });

  fetchPinnedUrl = path: builtins.fetchurl (readJSON path);

  hs = pkgs.haskellPackages;

  allowNewer = pkgs.haskell.lib.doJailbreak;

  skipTests = pkgs.haskell.lib.dontCheck;
in
rec {
  all-hies = import (fetchPinnedGitHub nix/all-hies.json) {};

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
      all-hies
      codex
      emacs
      jdt-language-server
      mwebster-1913
    ;
    ripgrep = pkgs.ripgrep.override { withPCRE2 = true; };
  };
}
