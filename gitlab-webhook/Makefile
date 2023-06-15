.PHONY: build push push_inputs push_outputs push_shell

CACHIX_CONFIG ?= $(HOME)/.config/cachix/cachix.dhall
NIX ?= nix

build:
	$(NIX) build

push: push_inputs push_outputs push_shell
push_inputs:
	$(NIX) flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' | cachix push spuriobot
push_outputs:
	$(NIX) build --json | jq -r '.[].outputs | to_entries[].value' | cachix push spuriobot
puush_shell:
	$(NIX) develop --profile dev-profile
	cachix -c $(CACHIX_CONFIG) push spuriobot dev-profile
