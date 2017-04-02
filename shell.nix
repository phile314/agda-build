with import <nixpkgs> {};

let
  agda-build = pkgs.haskellPackages.callPackage ./. {
    Agda = haskellPackages.callPackage ../agda2 {};
  };
in agda-build.env
