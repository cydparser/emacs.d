{ nixpkgs ? import <nixpkgs> {} }: with nixpkgs; {
  mx = callPackage ./. {};
}
