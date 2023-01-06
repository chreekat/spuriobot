with (import <nixpkgs> {});

haskellPackages.callPackage ./gitlab-webhook.nix {}
