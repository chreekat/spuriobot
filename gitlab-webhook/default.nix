{ pkgs ? import <nixpkgs> {}}:

with pkgs;

haskellPackages.callCabal2nix "gitlab-webhook" ./. {}
