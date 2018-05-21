{ nixpkgs ? import <nixpkgs> {} }: with nixpkgs; {
  emc = pkgs.callPackage ./. {};
}
