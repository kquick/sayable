{
  description = "Flake to build the haskell-src package 'sayable' and dependencies";

  nixConfig.bash-prompt-suffix = "sayable.env} ";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/23.11"; };
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    generic-deriving-src = {
      url = "https://hackage.haskell.org/package/generic-deriving-1.14.5/generic-deriving-1.14.5.tar.gz";
      flake = false;
    };
    tasty-ant-xml-src = {
      url = "https://hackage.haskell.org/package/tasty-ant-xml-1.1.9/tasty-ant-xml-1.1.9.tar.gz";
      flake = false;
    };
    th-abstraction-src = {
      url = "https://hackage.haskell.org/package/th-abstraction-0.6.0.0/th-abstraction-0.6.0.0.tar.gz";
      flake = false;
    };
  };

  outputs = { self, levers, nixpkgs
            , generic-deriving-src
            , tasty-ant-xml-src
            , th-abstraction-src
            }:
    let
      shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
        { buildInputs = old.buildInputs ++ adds pkgs; });
      # Add additional packages useful for a development shell, generally
      # representing test packages or non-propagated build dependencies of
      # various sub-packages.
      shellPkgs = pkgs: [
        # pkgs.haskell.compiler.integer-simple.ghc8107
        # pkgs.haskell.packages.ghc8107.profiteur
        pkgs.cabal-install
      ];
    in rec {
      devShells =
        let oneshell = s: n:
              let pkgs = import nixpkgs { system=s; };
              in levers.variedTargets
                { ghcver = levers.validGHCVersions pkgs.haskell.compiler; }
                ( { ghcver, ... } @ vargs:
                  shellWith pkgs shellPkgs
                    (self.packages.${s}.${n}.${ghcver}.env.overrideAttrs (a:
                      {
                        # Set envvars here
                      }
                    )));
        in levers.eachSystem
          (s:
            let pkgs = import nixpkgs { system=s; };
                names = builtins.attrNames (self.packages.${s});
                outs = builtins.removeAttrs (pkgs.lib.genAttrs names (oneshell s))
                  [ "ghc" ];
                shells = pkgs.lib.attrsets.mapAttrs (n: v: v.default) outs;
            in shells // { default = devShells.${s}.sayable_tests; }
          ) ;

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg {
            inherit nixpkgs system;
            ghcver = levers.validGHCVersions pkgs.haskell.compiler;
            };
          pkgs = import nixpkgs { inherit system; };
          wrap = levers.pkg_wrapper system pkgs;
          haskellAdj = drv:
            with (pkgs.haskell).lib;
            dontHaddock (dontCheck (dontBenchmark drv));
        in rec {
          ghc = pkgs.haskell.compiler.ghc8107;
          default = sayable;
          TESTS = wrap "Sayable-TESTS" [ sayable_tests ];
          DOC = wrap "Sayable-DOC" [ sayable_doc ];
          generic-deriving = mkHaskell "generic-deriving" generic-deriving-src {
            # Override build because newer th-abstraction needed (see below).
            inherit th-abstraction;
          };
          tasty-ant-xml = mkHaskell "tasty-ant-xml" tasty-ant-xml-src {
            # Override build needed because this depends on generic-deriving,
            # which depends on th-abstraction, which is overriden below.
            inherit generic-deriving;
          };
          th-abstraction = mkHaskell "th-abstraction" th-abstraction-src {
            # Override build needed because nixos-23.11 version is older and does
            # not support GHC 9.8.
          };
          sayable = mkHaskell "sayable" self {
            inherit th-abstraction;
            adjustDrv = args:
              drv:
                haskellAdj drv;
            };
          sayable_tests = mkHaskell "sayable_tests" self {
            inherit tasty-ant-xml th-abstraction;
            adjustDrv = args:
              drv:
                pkgs.haskell.lib.doBenchmark (pkgs.haskell.lib.doCheck (haskellAdj drv));
            };
          sayable_doc = mkHaskell "sayable_doc" self {
            inherit th-abstraction;
            adjustDrv = args:
              drv:
              pkgs.haskell.lib.doHaddock
                (haskellAdj drv);
            };
        });
    };
}
