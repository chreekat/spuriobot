{
  nixConfig = {
    extra-substituters = [ "https://spuriobot.cachix.org" ];
    extra-trusted-public-keys = [ "spuriobot.cachix.org-1:npiBX491xE1n5qVRBBtqutI0PzJV2OeF9dCgibklr3c=" ];
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  outputs = { self, nixpkgs, ... }:
    let
      hsOverlay = pkgs: self: super: {
        spuriobot = pkgs.lib.pipe (self.callCabal2nix "spuriobot" ./spuriobot {}) [
          pkgs.haskell.lib.compose.disableExecutableProfiling
          pkgs.haskell.lib.compose.justStaticExecutables
          (pkgs.haskell.lib.compose.appendConfigureFlags ["--verbose=1"])
          pkgs.haskell.lib.compose.failOnAllWarnings
        ];
        # responseLinks
        req = pkgs.lib.pipe super.req [
          (pkgs.haskell.lib.compose.overrideSrc
            { src = pkgs.fetchFromSourcehut {
                owner = "~chreekat";
                repo = "req";
                rev = "793e691b50bd26873f34049bc075fd2a01339ccd";
                sha256 = "sha256-Mmdthc6Moijm2v5QkFZvOIbjwgGNFXODWezazYkicyk=";
              };
            }
          )
          pkgs.haskell.lib.compose.doJailbreak
        ];
        scotty = self.callHackage "scotty" "0.22" {};
      };
      myPkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
    in {
      overlays.default = final: prev: {
        spuriobotHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };

        myShell = final.spuriobotHaskellPackages.shellFor {
          packages = pkgs: [ pkgs.spuriobot ];
          buildInputs = [ myPkgs.haskell-language-server myPkgs.cachix final.nil final.postgresql final.stylish-haskell ];
        };
      };
      nixosModules.default = ./nix-packaging/spuriobot-systemd-module.nix;

      devShells.x86_64-linux.default = myPkgs.myShell;
      packages.x86_64-linux.default = myPkgs.spuriobotHaskellPackages.spuriobot;
      apps.x86_64-linux.default = {
        type = "app";
        program = myPkgs.lib.getExe self.packages.x86_64-linux.default;
      };
    };
}
