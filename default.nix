let
  readJSON = path: builtins.fromJSON (builtins.readFile path);

  fetchPinnedUrl = path: builtins.fetchurl (readJSON path);

  pkgs = import (builtins.fetchTarball (readJSON nix/nixpkgs.json)) {};

  inherit (pkgs) callPackage;

  fetchPinnedGitHub = path: pkgs.fetchFromGitHub ((readJSON path) // { fetchSubmodules = true; });

  inherit (pkgs.haskellPackages) apply-refact hlint;

in
rec {
  delta = pkgs.gitAndTools.delta;

  emacs =
    let
      emacsCustom = pkgs.emacs.override {
        nativeComp = true;
        webkitgtk = pkgs.webkitgtk;
        # withPgtk = true;
        withXinput2 = true;
        withXwidgets = true;
      };
    in (pkgs.emacsPackagesFor emacsCustom).emacsWithPackages (epkgs: with epkgs.melpaStablePackages; [
      pdf-tools
    ]);

  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix {
    inherit
      apply-refact
      delta
      emacs
      hlint
      mwebster-1913
    ;
    ripgrep = pkgs.ripgrep.override { withPCRE2 = true; };
  };
}
