{ sources ? import ./nix/sources.nix }:

let pkgs = import sources.nixpkgs { overlays = []; config = {}; };
in
pkgs.haskellPackages.callCabal2nix "gitlab-webhook" ./. {}
