let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          ask-for = pkgs.haskellPackages.callPackage ./ask-for.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in { ask-for = pkgs.haskellPackages.ask-for; }
