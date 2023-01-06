{
  description = "Haskell Gitlab webhook for spurious failures";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        gitlab-webhook-exe-drv = pkgs.haskell.packages.ghc924.callCabal2nix "gitlab-webhook-exe" self { };
      in
      {
        packages.gitlab-webhook-exe =
            pkgs.haskell.lib.justStaticExecutables gitlab-webhook-exe-drv;

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.gitlab-webhook-exe}/bin/gitlab-webhook-exe";
        };
      });
}
