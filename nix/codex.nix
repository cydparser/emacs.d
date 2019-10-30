{ fetchPinnedGitHub }:
let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/c75de8bc12cc7e713206199e5ca30b224e295041.tar.gz";
    sha256 = "1awipcjfvs354spzj2la1nzmi9rh2ci2mdapzf4kkabf58ilra6x";
  }) {};
in
pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.callCabal2nix "codex" (fetchPinnedGitHub ./codex.json) {})
