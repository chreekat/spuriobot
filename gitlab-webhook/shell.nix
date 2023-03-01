{ sources ? import ./nix/sources.nix }:

let
  pkgs = import sources.nixpkgs { overlays = []; config = {}; };
  hp = pkgs.haskellPackages;
in

hp.shellFor {
  packages = p: [(import ./. {})];
  withHoogle = true;
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell-language-server
    pkgs.haskellPackages.fourmolu
  ];
}
