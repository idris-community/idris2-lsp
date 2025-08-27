{
  description = "Idris 2 Language Server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    idris = {
      url = "github:idris-lang/Idris2";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lspLib = {
      url = "github:idris-community/LSP-lib";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.idris.follows = "idris";
      inputs.idris2Lsp.follows = "idris2Lsp";
    };

    alejandra = {
      url = "github:kamadorueda/alejandra/3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # chicken-and-egg situation: we don't want the version of the
    # LSP we use to develop this package to use the version of Idris
    # we are currently developing against or else you could not use
    # a functioning LSP while addressing breaking changes.
    idris2Lsp = {
      url = "github:idris-community/idris2-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.alejandra.follows = "alejandra";

      # This stops the flake system from recursing over and over until the earliest idris2Lsp.
      # It does that by breaking the recursion by introducing a **fake** idris2Lsp for the next
      # level down. It will not break anything, since idris2Lsp doesn't use idris2Lsp to build
      # itself, it is only here to be provided in the developer shell
      inputs.idris2Lsp.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    alejandra,
    idris,
    lspLib,
    idris2Lsp,
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
        lspLib' = lspLib.packages.${system}.default;
        supportLibrariesPath = lib.makeLibraryPath [idrisPkgs.support];
        supportSharePath = lib.makeSearchPath "share" [idrisPkgs.support];

        globalLibraries = let
          idrName = "idris2-${idris.version}";
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
          idrisLibraries = [idrisPkgs.idris2Api lspLib'];
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
        devShells = {
          default = nixpkgs.legacyPackages.${system}.mkShell {
            inputsFrom = [self.packages.${system}.default];
            packages = [
              idris2Lsp.packages.${system}.default
              idrisPkgs.idris2
            ];
          };
        };
      }
    );
}
