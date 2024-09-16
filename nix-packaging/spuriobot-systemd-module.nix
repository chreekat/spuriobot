{ config, lib, pkgs, ... }:
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
          ExecStart = pkgs.lib.getExe pkgs.spuriobotHaskellPackages.spuriobot;
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
  }

