{
  nixConfig = {
    extra-substituters = [ "https://spuriobot.cachix.org" ];
    extra-trusted-public-keys = [ "spuriobot.cachix.org-1:npiBX491xE1n5qVRBBtqutI0PzJV2OeF9dCgibklr3c=" ];
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  outputs = { self, nixpkgs, ... }:
    let
      hsOverlay = pkgs: self: super: {
        spuriobot = pkgs.haskell.lib.compose.disableExecutableProfiling
          (pkgs.haskell.lib.compose.justStaticExecutables
            (pkgs.haskell.lib.compose.appendConfigureFlags ["--verbose=1"]
              (self.callCabal2nix "spuriobot" ./spuriobot {})));
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
      };
      myPkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
    in {
      overlays.default = final: prev: {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };

        myShell = final.myHaskellPackages.shellFor {
          packages = pkgs: [ pkgs.spuriobot ];
          buildInputs = [ myPkgs.haskell-language-server myPkgs.cachix final.nil final.postgresql final.stylish-haskell ];
        };
      };
      nixosModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.services.spuriobot;
          botOptions = { lib, ... }: {
            options = {
              enable = lib.mkEnableOption "spuriobot";

              envFile = lib.mkOption {
                type = lib.types.str;
                description = ''
                  Path, as a string, to an EnvironmentFile (see systemd.exec).

                  Although it's a bad idea, this is used for passing secrets.
                  (FIXME: Use LoadCredential instead.)
                '';
              };

              database = lib.mkOption {
                type = lib.types.str;
                description = ''
                  Name of the Postgresql database to connect to.

                  You can also skip this and just set PGDATABASE in the
                  environment via the `envFile` option.

                  **The database must already exist. The table 'ci_failure' must
                  already exist within it. And the user 'spuriobot' must
                  have write access to that table.**

                  (FIXME: The above)
                '';
              };
            };
          };
        in {
          options.services.spuriobot = lib.mkOption {
            type = lib.types.submodule botOptions;
            default = {};
            description = ''
              Configuration for spuriobot, the GitLab spurious failure webhook
              service
            '';
          };
          config = {
            systemd.services.spuriobot = lib.mkIf cfg.enable {
              description = "GitLab spurious failure webhook service";
              wantedBy = [ "network.target" ];
              environment = {
                PGDATABASE = cfg.database;
              };
              serviceConfig = {
                # Use myPkgs here because we need a spanking new version of
                # 'req', which isn't on the one-and-only target machine this flake
                # is being written for. Otherwise it would be better(?) to
                # include our overlay in this config and pick up the package via
                # the standard 'pkgs'. This would allow the client system to use
                # the same nixpkgs throughout automatically, instead of pulling
                # in a second one through myPkgs.
                #
                # Alternatively, we could specify the right version of req in
                # our overlay, but then users would have no (easy) way to
                # override it. At least this way, users can override the nixpkgs
                # used by this flake (at their own risk).
                #
                # If we stop supporting NixOS < 23.05, we can get rid of all
                # this.
                ExecStart = pkgs.lib.getExe myPkgs.myHaskellPackages.spuriobot;
                EnvironmentFile = cfg.envFile;
                User = "spuriobot";
                DynamicUser = "yes";
                Restart = "on-failure";
              };
              # With these settings, I want to make sure the bot dies only if
              # it's *really* misbehaving. But in general, I don't have good
              # intuition for what these values should be.
              serviceConfig.RestartSec = 1;
              startLimitIntervalSec = 30;
              startLimitBurst = 20;
            };
          };
        };

      devShells.x86_64-linux.default = myPkgs.myShell;
      packages.x86_64-linux.default = myPkgs.myHaskellPackages.spuriobot;
      apps.x86_64-linux.default = {
        type = "app";
        program = myPkgs.lib.getExe self.packages.x86_64-linux.default;
      };
    };
}
