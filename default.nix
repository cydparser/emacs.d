{ pkgs ? import <nixpkgs> {} }: rec {
  inherit (pkgs) callPackage;

  codex = callPackage nix/codex.nix {};

  hies = callPackage nix/hies.nix {};

  nix-linter = callPackage nix/nix-linter.nix {};
  jdt-language-server = callPackage nix/jdt-language-server.nix { jre = pkgs.jre10_headless; inherit fetchPinnedUrl; };


  mwebster-1913 = callPackage nix/mwebster-1913.nix {};

  mx = callPackage nix/mx.nix { inherit codex hies jdt-language-server nix-linter mwebster-1913; };
}
