{ pkgs ? import <nixpkgs> {} }:
(pkgs.haskell.lib.addBuildTools
  (import ./. { inherit pkgs; })
  [
  pkgs.cabal-install
  pkgs.haskell-language-server
  pkgs.haskellPackages.fourmolu
  ]
  ).env
