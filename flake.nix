{
  description = "Idris 2 Language Server";

  inputs.idris = {
    # tmp url:
    url = "github:mattpolzin/Idris2/nix-idrisapi";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, idris }:
    let
      # support the same systems as Idris2
      systems = builtins.attrNames idris.packages;
    in
    { packages = builtins.genAttrs systems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          idrisPkgs = idris.packages.${system};
          buildIdris = idris.buildIdris.${system};

          lspPkg = buildIdris {
            projectName = "lsp";
            src = ./.;
            idrisLibraries = [ idris.idris2-api ];
            buildInputs = [ pkgs.makeWrapper ];
            postInstall = ''
            wrapProgram $out/bin/idris2-lsp --prefix IDRIS2_PACKAGE_PATH : ${lib-dirs}
            '';
          };
        in rec {
          lsp = lspPkg.executable; 
          default = lsp;
        } // idrisPkgs
      );
    };
}
