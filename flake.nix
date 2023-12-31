{
  description = "Idris 2 Language Server";

  inputs.idris = {
    # tmp url:
    url = "github:mattpolzin/Idris2/nix-idrisapi";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.lsp-lib = {
    # tmp url:
    url = "github:mattpolzin/LSP-lib/nix-flake";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.idris.follows = "idris";
  };

  outputs = { self, nixpkgs, idris, lsp-lib }:
    let
      lib = nixpkgs.lib;
      # support the same systems as Idris2
      systems = builtins.attrNames idris.packages;
    in
    { packages = lib.genAttrs systems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          idrisPkgs = idris.packages.${system};
          buildIdris = idris.buildIdris.${system};
          lspLibPkg = lsp-lib.packages.${system};
          supportLibrariesPath = lib.makeLibraryPath [ idrisPkgs.support ];
          supportSharePath = lib.makeSearchPath "share" [ idrisPkgs.support ];

          globalLibraries =
            let 
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
            idrisLibraries = [ idrisPkgs.idris2-api lspLibPkg.lsp-lib ];
            buildInputs = [ pkgs.makeWrapper ];
            postInstall = ''
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
          lsp = lspPkg.executable; 
          default = lsp;
        } // idrisPkgs
      );
    };
}
