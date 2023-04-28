{
  outputs = { self, nixpkgs, ... }:
    let

      hsOverlay = pkgs: self: super: {
        spuriobot = self.callCabal2nix "spuriobot" ./. {};
      };
      pkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlay ]; };
    in {
      overlay = self: super: {
        myHaskellPackages = super.haskellPackages.override {
          overrides = hsOverlay self;
        };

        myShell = self.myHaskellPackages.shellFor {
          packages = pkgs: [ pkgs.spuriobot ];
        };
      };
      nixosModules.default = { pkgs, ... }: {
        systemd.services.spuriobot = {
          description = "GitLab spurious failure webhook service";
          wantedBy = [ "multi-user.target" ];
          serviceConfig.ExecStart = pkgs.lib.getExe pkgs.myHaskellPackages.spuriobot;
        };
      };
      devShell.x86_64-linux = pkgs.myShell;
      packages.x86_64-linux.spuriobot = pkgs.myHaskellPackages.spuriobot;
      apps.x86_64-linux.spuriobot = {
        type = "app";
        program = self.packages.x86_64-linux.spuriobot;
      };

      # defaultPackage.x86_64-linux = self.packages.x86_64-linux.spuriobot;
      # defaultApp.x86_64-linux = self.apps.x86_64-linux.spuriobot;
    };
}
