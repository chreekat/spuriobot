{ sources ? import ./nix/sources.nix
}:
let
    np = sources.nixpkgs;

    pkgs = import np { inherit config; };

    config = {
      packageOverrides = pkgs: {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = extensions;
        };
      };
    };

    h = pkgs.haskell.lib;
    hp = pkgs.haskellPackages;

    localSrc = pkgs.lib.cleanSourceWith {
      filter = name: _type: baseNameOf (toString name) != "jobs.db";
      src = pkgs.lib.cleanSource ./.;
    };

    customizedDeps = self: super: {
      # Need dat generated columns yo
      direct-sqlite =
        self.callHackageDirect {
          pkg = "direct-sqlite";
          ver = "2.3.27";
          sha256 = "sha256-N8KJ2spJJEnbHGj+MsygUT+mZ4sQA6I5xQhAEMSQHHE=";
      } {};

      # responseLinks
      req =
        h.doJailbreak (self.callCabal2nix
          "req"
          (pkgs.fetchFromSourcehut {
            owner = "~chreekat";
            repo = "req";
            rev = "bbe1e206d5be56238534992d5af96b9709f24f56";
            sha256 = "sha256:0pcb850ikbvg1h42dhz376m62x3kjjxfwk034096fjm33j3v5awl";
          })
          {});
    };

    ownPackages = self: super: {
      spurious-failures =
        self.callCabal2nix
          "spurious-failures"
          localSrc
          {};
    };

    extensions = self: super:
      customizedDeps self super // ownPackages self super;

in {
  inherit (hp) spurious-failures;
}
