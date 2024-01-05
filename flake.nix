{
  description = "Idris 2 Language Server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    idris = {
      url = "github:idris-lang/Idris2";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lsp-lib = {
      # tmp url:
      url = "github:mattpolzin/LSP-lib/nix-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.idris.follows = "idris";
    };

    alejandra = {
      url = "github:kamadorueda/alejandra/3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    alejandra,
    idris,
    lsp-lib,
  }: let
    lib = nixpkgs.lib;
    # support the same systems as Idris2
    systems = builtins.attrNames idris.packages;
    forEachSystem = with lib; mkOutputs: let
      outputsForSystem = system:
        concatMapAttrs (k: v: {${k}.${system} = v;}) (mkOutputs system);
    in
      foldl' recursiveUpdate {} (map outputsForSystem systems);
  in
    forEachSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        idrisPkgs = idris.packages.${system};
        buildIdris = idris.buildIdris.${system};
        lspLibPkg = lsp-lib.packages.${system};
        supportLibrariesPath = lib.makeLibraryPath [idrisPkgs.support];
        supportSharePath = lib.makeSearchPath "share" [idrisPkgs.support];

        globalLibraries = let
          idrName = "idris2-${idris.version}";
          libSuffix = "lib/${idrName}";
        in [
          "\\$HOME/.nix-profile/lib/${idrName}"
          "/run/current-system/sw/lib/${idrName}"
          "${idrisPkgs.idris2}/${idrName}"
        ];
        globalLibrariesPath = builtins.concatStringsSep ":" globalLibraries;

        lspPkg = buildIdris {
          projectName = "idris2-lsp";
          src = ./.;
          idrisLibraries = [idrisPkgs.idris2-api lspLibPkg.lsp-lib];
          buildInputs = [pkgs.makeWrapper];
          postInstall = ''
            # TODO: upstream some of this installation cleanup to the buildIdris helper.
            # Not all of these ENV var modifications are always needed, but the juggling
            # of files in the next 3 lines and the inclusion of the support lib in the
            # LD_LIBRARY_PATH is always going to be important I believe.
            rm $out/bin/idris2-lsp
            mv $out/bin/idris2-lsp_app/idris2-lsp.so $out/bin/idris2-lsp
            rm -rf $out/bin/idris2-lsp_app

            wrapProgram $out/bin/idris2-lsp \
              --run 'export IDRIS2_PREFIX=''${IDRIS2_PREFIX-"$HOME/.idris2"}' \
              --suffix IDRIS2_LIBS ':' "${supportLibrariesPath}" \
              --suffix IDRIS2_DATA ':' "${supportSharePath}" \
              --suffix IDRIS2_PACKAGE_PATH ':' "${globalLibrariesPath}" \
              --suffix LD_LIBRARY_PATH ':' "${supportLibrariesPath}" \
              --suffix DYLD_LIBRARY_PATH ':' "${supportLibrariesPath}" \
          '';
        };
      in rec {
        packages =
          rec {
            idris2-lsp = lspPkg.executable;
            default = idris2-lsp;
          }
          // idrisPkgs;
        formatter = alejandra.packages.${system}.default;
      }
    );
}
