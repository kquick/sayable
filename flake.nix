{
  # The TESTS and DOC targets build and test the current, the minimum, and the
  # maximum supported GHC version.  To update the GHC version, modify those
  # targets below.

  description = "Flake to build the haskell-src package 'sayable' and dependencies";

  nixConfig.bash-prompt-suffix = "sayable.env} ";

  inputs = {
    nixpkgs_unstable = { url = "github:nixos/nixpkgs"; };
    nixpkgs.url = "github:nixos/nixpkgs/23.11";
    nixpkgs2505.url = "github:nixos/nixpkgs/25.05";
    nixpkgs2009.url = "github:nixos/nixpkgs/20.09";  # event horizon for flakes. ghc865
    # Supported nixpkgs versions should be added to `nixpkgs_list` below.
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, levers, nixpkgs, nixpkgs2505, nixpkgs2009, nixpkgs_unstable
            # , tasty-ant-xml-src
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
          nixpkgs_list = [
            nixpkgs_unstable
            nixpkgs2009
            nixpkgs2505
            nixpkgs
          ]; # prefer pkgs from earlier entries
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
            sayable_tests         # default version
            sayable_tests.ghc88   # min supported version
            sayable_tests.ghc912  # max supported version
          ];
          DOC = wrap "Sayable-DOC" [
            sayable_doc         # default version
            sayable_doc.ghc88   # min supported version
            sayable_doc.ghc912  # max supported version
          ];
          sayable = mkHaskell "sayable" self {
            adjustDrv = args: haskellAdj;
            };
          sayable_tests = mkHaskell "sayable_tests" self {
            adjustDrv = args: d:
              with (pkgs.haskell).lib;
              let n = d.overrideAttrs (old: {
                    postInstallHook = builtins.trace "Completed for ${args.ghcver}" "echo good";
                  });
              in doBenchmark (doCheck (haskellAdj n));
            };
          sayable_doc = mkHaskell "sayable_doc" self {
            adjustDrv = args: drv:
              pkgs.haskell.lib.doHaddock (haskellAdj drv);
            };
        });
    };
}
