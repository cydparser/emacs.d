{ fetchPinnedUrl, jre, stdenv }:
stdenv.mkDerivation {
  name = "jdt-language-server";

  src = fetchPinnedUrl ./jdt-language-server.json;

  sourceRoot = ".";

  installPhase = ''
    mkdir -p $out/jre
    mv ./* $out/
    ln -s ${jre}/bin $out/jre/
  '';
}
