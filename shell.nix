let
  pkgs    = import <nixpkgs> {};
  gittery = pkgs.haskellPackages.callCabal2nix "gittery" ./. {};
  shell   = pkgs.haskellPackages.shellFor {
    packages = _: [ gittery ];
  };
in
  shell
