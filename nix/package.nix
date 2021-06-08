{ stdenv, lib, chez, idris2 }:

stdenv.mkDerivation {
  pname = "idris2-lsp";
  version = "unstable-06-06-2021";

  src = ../.;

  buildInputs = [ chez idris2 ];

  buildFlags = [ "build" ];

  installPhase = ''
    echo $(ls build/exec/idris2-lsp_app)
    echo $(ls build/ttc)
    echo "-----------------------------"
    mkdir -p $out/bin
    install build/exec/idris2-lsp $out/bin
    mv build/exec/idris2-lsp_app $out/bin/idris2-lsp_app
    # install build/exec/idris2-lsp_app/idris2-lsp.so $out/bin/idris2-lsp_app
  '';
}
