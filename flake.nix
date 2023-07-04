{
  description = "Idris 2 Language Server";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.idris = {
    url = "github:idris-lang/Idris2";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, idris, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        npkgs = import nixpkgs { inherit system; };
        idrisPkgs = idris.packages.${system};
        buildIdris = idris.buildIdris.${system};

        #from https://github.com/claymager/idris2-pkgs/blob/main/packageSet.nix
        #should probably be upstreamed to Idris
        idris2-api = buildIdris {
          projectName = "idris2api";
          src = idris;
          idrisLibraries = [ ];
          dontBuild = true;
          postConfigure = 
          let 
            versionString = builtins.replaceStrings ["."] [","] idris.version; #replace all "." by ","
            gitRev = "ecf4765c4";
          in
          ''
          echo "-- @""generated" > src/IdrisPaths.idr
          echo 'module IdrisPaths' >> src/IdrisPaths.idr
          echo 'export idrisVersion : ((Nat,Nat,Nat), String); idrisVersion = ((${versionString}), "${gitRev}")' >> src/IdrisPaths.idr
          echo 'export yprefix : String; yprefix="./lib"' >> src/IdrisPaths.idr
          '';
        };


        contrib = buildIdris {
          projectName = "contrib";
          src = idris + "/libs/contrib";
          idrisLibraries = [ ];
        };
        idris2-prelude = buildIdris {
          projectName = "prelude";
          src = idris + "/libs/prelude";
          idrisLibraries = [ ];
        };
        idris2-base = buildIdris {
          projectName = "base";
          src = idris + "/libs/base";
          idrisLibraries = [ ];
        };

        # libs that are needed at runtime
        # something like this should also probably be upstreamed to Idris
        runtimeLibs = [ idris2-prelude.installLibrary idris2-base.installLibrary ];
         
        lib-dirs =
        let 
          idrName = "idris2-${idris.version}";
          libSuffix = "lib/${idrName}";
        in
          nixpkgs.lib.strings.concatMapStringsSep ":" (p: "${p}/${libSuffix}") runtimeLibs;

        pkgs = buildIdris {
          projectName = "lsp";
          src = ./.;
          idrisLibraries = [ contrib.installLibrary idris2-api.installLibrary ];
          buildInputs = runtimeLibs ++ [npkgs.makeWrapper];
          postInstall = ''
          wrapProgram $out/bin/idris2-lsp --prefix IDRIS2_PACKAGE_PATH : ${lib-dirs}
          '';
        };
      in rec {
        packages = pkgs // idrisPkgs;
        defaultPackage = pkgs.build;
        devShell = npkgs.mkShell {
          buildInputs = [ idrisPkgs.idris2 npkgs.rlwrap ];
          shellHook = ''
            alias idris2="rlwrap -s 1000 idris2 --no-banner"
          '';
        };
      });
}
