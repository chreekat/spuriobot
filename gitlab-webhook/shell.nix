{ pkgs ? import <nixpkgs> {} }:
(pkgs.haskell.lib.addBuildTools
  (import ./. { inherit pkgs; })
  [
  pkgs.cabal-install
  pkgs.haskell-language-server
  pkgs.haskellPackages.fourmolu
  ]
  ).env


#with (import <nixpkgs> {});
#
#let gitlab-webhook-drv = haskellPackages.callCabal2nix "gitlab-webhook" ./. {};
#in
#  mkShell {
#    nativeBuildInputs = [
#      gitlab-webhook-drv.compiler
#      haskell-language-server # probably wrong, needs to match the compiler above
#      cabal-install
#    ];
#    buildInputs = gitlab-webhook-drv.buildInputs;
#  }
