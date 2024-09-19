{
  # Note: contains overrides for building with GHC 9.8 because the nixpkgs
  # specifications for GHC 9.8 do not reference a compatible version of
  # th-abstraction.

  description = "Flake to build the haskell-src package 'sayable' and dependencies";

  nixConfig.bash-prompt-suffix = "sayable.env} ";

  inputs = {
    nixpkgs_unstable = { url = "github:nixos/nixpkgs"; };
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

  outputs = { self, levers, nixpkgs, nixpkgs_unstable
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
          pkgs = import nixpkgs { inherit system; };
          nixpkgs_list = [ nixpkgs nixpkgs_unstable ]; # prefer pkgs from earlier entries
          mkHaskell = name: src: ovrDrvOrArgs:
            levers.mkHaskellPkgs { inherit system; }
              nixpkgs_list name src ovrDrvOrArgs;
          wrap = levers.pkg_wrapper system pkgs;
          haskellAdj = drv:
            # Used to make standard adjustments to a haskell package
            with (pkgs.haskell).lib;
            dontHaddock (dontCheck (dontBenchmark drv));
        in rec {
          default = sayable;
          TESTS = wrap "Sayable-TESTS" [
            sayable_tests
            sayable_tests.ghc810  # min supported version
            sayable_tests.ghc910  # max supported version
          ];
          DOC = wrap "Sayable-DOC" [
            sayable_doc
            sayable_doc.ghc810  # min supported version
            sayable_doc.ghc910  # max supported version
          ];
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
            adjustDrv = args:
              drv:
              if args.ghcver == "ghc98"
              then haskellAdj
                (drv.override
                  {
                    th-abstraction = builtins.getAttr args.ghcver th-abstraction;
                  })
              else haskellAdj drv;
            };
          sayable_tests = mkHaskell "sayable_tests" self {
            adjustDrv = args:
              drv:
              let mod = d:
                    with (pkgs.haskell).lib;
                    doBenchmark (doCheck (haskellAdj d));
              in if args.ghcver == "ghc98"
                 then mod (drv.override
                   {
                     th-abstraction = builtins.getAttr args.ghcver th-abstraction;
                     tasty-ant-xml = builtins.getAttr args.ghcver tasty-ant-xml;
                   })
                 else mod drv;
            };
          sayable_doc = mkHaskell "sayable_doc" self {
            adjustDrv = args:
              drv:
              let mod = d: pkgs.haskell.lib.doHaddock (haskellAdj d);
              in if args.ghcver == "ghc98"
                 then mod (drv.override
                   {
                     th-abstraction = builtins.getAttr args.ghcver th-abstraction;
                   })
                 else mod drv;
            };
        });
    };
}
