{
  outputs = { self, nixpkgs, ... }:
    let

      hsOverlay = pkgs: self: super: {
        spuriobot = self.callCabal2nix "spuriobot" ./. {};
      };
      pkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
    in {
      overlays.default = final: prev: {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };

        myShell = final.myHaskellPackages.shellFor {
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
      devShells.x86_64-linux.default = pkgs.myShell;
      packages.x86_64-linux.default = pkgs.myHaskellPackages.spuriobot;
      apps.x86_64-linux.default = {
        type = "app";
        program = pkgs.lib.getExe self.packages.x86_64-linux.default;
      };
    };
}
