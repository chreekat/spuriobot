#!/usr/bin/env nix-shell
#! nix-shell -i bash -p cachix gnumake jq

set -Eeuo pipefail

export NIX="nix --extra-experimental-features nix-command --extra-experimental-features flakes --accept-flake-config"

case "$1" in
    "spuriobot_nix_build")
        make -C gitlab-webhook build
        ;;
    "spuriobot_nix_build_and_cachix_push")
        make -C gitlab-webhook push
        ;;
esac
