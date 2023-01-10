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

    localSrc = pkgs.lib.cleanSource;

    extensions = self: super: {
      spurious-failures =
        self.callCabal2nix
          "spurious-failures"
          (localSrc ./.)
          {};
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

in {
  inherit (hp) spurious-failures;
}
