{
  # inputs.nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
  outputs = { self, nixpkgs, ... }:
    let
      hsOverlay = pkgs: self: super: {
        myPackage = self.callCabal2nix "myPackage" ./. {};

        # Need dat generated columns yo
        direct-sqlite =
          self.callHackageDirect {
            pkg = "direct-sqlite";
            ver = "2.3.27";
            sha256 = "sha256-N8KJ2spJJEnbHGj+MsygUT+mZ4sQA6I5xQhAEMSQHHE=";
        } {};

        # responseLinks
        req =
          pkgs.haskell.lib.doJailbreak (self.callCabal2nix
            "req"
            (pkgs.fetchFromSourcehut {
              owner = "~chreekat";
              repo = "req";
              rev = "bbe1e206d5be56238534992d5af96b9709f24f56";
              sha256 = "sha256:0pcb850ikbvg1h42dhz376m62x3kjjxfwk034096fjm33j3v5awl";
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
