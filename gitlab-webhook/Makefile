.PHONY: build push push_inputs push_outputs push_shell

CACHIX_CONFIG ?= $(HOME)/.config/cachix/cachix.dhall
CACHIX ?= cachix -c $(CACHIX_CONFIG)
NIX ?= nix

build:
	$(NIX) build

push: push_inputs push_outputs push_shell
push_inputs:
	$(NIX) flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' | $(CACHIX) push spuriobot
push_outputs:
	$(NIX) build --json | jq -r '.[].outputs | to_entries[].value' | $(CACHIX) push spuriobot
puush_shell:
	$(NIX) develop --profile dev-profile
	$(CACHIX) push spuriobot dev-profile
