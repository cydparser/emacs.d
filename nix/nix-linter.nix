{ fetchFromGitHub, pkgs }:
let
  src = fetchFromGitHub {
    owner = "Synthetica9";
    repo = "nix-linter";
    rev = "31666b492bfacb58149a2efdcfbeec73b01b44a2";
    sha256 = "02ssc6f6rvww9n6n4ydcnygk75mhqzcx3n80p8cw9kb0zccdqcn4";
  };
in
  (import src { inherit pkgs; }).nix-linter
