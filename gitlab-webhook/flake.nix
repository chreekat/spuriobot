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
      nixosModules.default = { config, lib, pkgs, ... }:
        let cfg = config.services.spuriobot;
        in {
          options.services.spuriobot = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = ''
                Enable spuriobot, the GitLab webhook service that retries
                spurious failures.
              '';
            };

            envFile = lib.mkOption {
              type = lib.types.str;
              description = ''
                Path, as a string, to an EnvironmentFile (see systemd.exec).
                Used for passing secrets.
              '';
            };
          };
          config = {
            systemd.services.spuriobot = lib.mkIf cfg.enable {
              description = "GitLab spurious failure webhook service";
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                ExecStart = pkgs.lib.getExe pkgs.myHaskellPackages.spuriobot;
                EnvironmentFile = cfg.envFile;
                User = "spuriobot";
                DynamicUser = "yes";
                SupplementaryGroups = [ "keys" ];
              };
            };
          };
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
