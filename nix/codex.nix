{ fetchFromGitHub, haskellPackages }:
let
  src = fetchFromGitHub {
    owner = "aloiscochard";
    repo = "codex";
    rev = "48b2a4b94132b537943bcd966c3922cbf67a7409";
    sha256 = "1mwsz824rqsphy68frqdrpkdmvlqg3zfn7wb565r8621jbbn9iws";
  };
in
  haskellPackages.callCabal2nix "codex" src {}