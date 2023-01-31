{ sources ? import ./nix/sources.nix }:

let pkgs = import sources.nixpkgs { overlays = []; config = {}; };
in

(pkgs.haskell.lib.addBuildTools
  (import ./. { inherit sources; })
  [
  pkgs.cabal-install
  pkgs.haskell-language-server
  pkgs.haskellPackages.fourmolu
  ]
  ).env
