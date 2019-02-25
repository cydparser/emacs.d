{ fetchPinnedUrl, jdk, makeWrapper, stdenv }:
stdenv.mkDerivation {
  name = "jdt-language-server";

  src = fetchPinnedUrl ./jdt-language-server.json;

  sourceRoot = ".";

  buildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin $out/share
    mv ./* $out/
    ln -s $out/plugins $out/share/java

    makeWrapper ${jdk}/bin/java $out/bin/lsp-java \
      --prefix PATH : ${stdenv.lib.makeBinPath [ jdk ]} \
      --set JAVA_HOME ${jdk.home}
  '';
}
