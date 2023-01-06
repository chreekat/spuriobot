{ pkgs ? import <nixpkgs> {} }:
(pkgs.haskell.lib.addBuildTool
  (import ./. { inherit pkgs; })
  pkgs.cabal-install
  ).env.overrideAttrs (o: { buildInputs = o.buildInputs ++ [ pkgs.haskell-language-server ];})


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
