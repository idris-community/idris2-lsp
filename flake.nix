{
  description = "Flake for the idris2 LSP";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  # The latest tested revision of Idris2 that works with idris2-lsp
  inputs.idris2flake.url =
    "github:idris-lang/Idris2/7aee7c9b7c50d16461e8d96d22d4ced2f19a52ef";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, idris2flake, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        idris2-with-api = idris2flake.defaultPackage.${system}.overrideAttrs
          (oldAttrs: rec {
            postInstall = oldAttrs.postInstall + ''
              export IDRIS2_BOOT="$out/bin/idris2"
              echo $IDRIS2_BOOT
              # Bootstrap Idris2
              make clean
              export PREFIX=$out
              make all && make install
              # Installing the Idris2 API
              make install-api
            '';
          });

      in rec {
        idris2-lsp = pkgs.callPackage ./nix/package.nix {
          stdenv = pkgs.stdenv;
          lib = pkgs.lib;
          chez = pkgs.chez;
          idris2 = idris2-with-api;
        };
        defaultPackage = idris2-lsp;
        devShell =
          pkgs.mkShell { buildInputs = [ pkgs.chez idris2-with-api ]; };

      });
}
