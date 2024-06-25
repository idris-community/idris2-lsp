{
  description = "Idris 2 Language Server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    idris = {
      url = "github:idris-lang/Idris2";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lsp-lib = {
      url = "github:idris-community/LSP-lib";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.idris.follows = "idris";
    };

    alejandra = {
      url = "github:kamadorueda/alejandra/3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    alejandra,
    idris,
    lsp-lib,
    ...
  }: let
    lib = nixpkgs.lib;
    # support the same systems as Idris2
    systems = builtins.attrNames idris.packages;
    forEachSystem = with lib;
      mkOutputs: let
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
        lspLib = lsp-lib.packages.${system}.default;
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
          ipkgName = "idris2-lsp";
          inherit (idris) version;
          src = ./.;
          idrisLibraries = [idrisPkgs.idris2Api lspLib];
          buildInputs = [pkgs.makeWrapper];
          postInstall = ''
            wrapProgram $out/bin/idris2-lsp \
              --run 'export IDRIS2_PREFIX=''${IDRIS2_PREFIX-"$HOME/.idris2"}' \
              --suffix IDRIS2_LIBS ':' "${supportLibrariesPath}" \
              --suffix IDRIS2_DATA ':' "${supportSharePath}" \
              --suffix IDRIS2_PACKAGE_PATH ':' "${globalLibrariesPath}"
          '';
        };
      in {
        packages =
          idrisPkgs
          // rec {
            idris2Lsp = lspPkg.executable;
            default = idris2Lsp;
          };
        formatter = alejandra.packages.${system}.default;
      }
    );
}
