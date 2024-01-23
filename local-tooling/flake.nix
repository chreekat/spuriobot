{

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  # flake-compat is unused in flake.nix, but is used in default.nix and
  # shell.nix
  outputs = { self, nixpkgs, flake-compat, ... }:
    let
      hsOverlay = pkgs: self: super: {
        myPackage = self.callCabal2nix "myPackage" ./. {};

        # responseLinks
        req =
          pkgs.haskell.lib.doJailbreak (self.callCabal2nix
            "req"
            (pkgs.fetchFromSourcehut {
              owner = "~chreekat";
              repo = "req";
              rev = "2792344f417f1627871299be077719f8a0460183";
              sha256 = "sha256-MjX8736L4a/ZxoXN4GVi4kZlGuOleniYb3sWax/5KhM=";
            })
            {});
      };
      myPkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
    in {
      overlays.default = final: prev: {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };

        myShell = final.myHaskellPackages.shellFor {
          packages = pkgs: [ pkgs.myPackage ];
          buildInputs = [ myPkgs.haskell-language-server myPkgs.cachix final.nil ];
          shellHook = ''PS1="\n∅ $PS1"'';
        };
      };

      devShells.x86_64-linux.default = myPkgs.myShell;
      packages.x86_64-linux.default = myPkgs.myHaskellPackages.myPackage;
      apps.x86_64-linux.default = {
        type = "app";
        program = myPkgs.lib.getExe self.packages.x86_64-linux.default;
      };
    };
}
