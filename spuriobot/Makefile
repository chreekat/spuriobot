.PHONY: test build push push_inputs push_outputs push_shell

CACHIX_CONFIG ?= $(HOME)/.config/cachix/cachix.dhall
CACHIX ?= cachix -c $(CACHIX_CONFIG)
NIX ?= nix

test:
	cabal test

build:
	$(NIX) build

push: push_inputs push_outputs push_shell
push_inputs:
	$(NIX) flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' | $(CACHIX) push spuriobot
push_outputs:
	$(NIX) build --json | jq -r '.[].outputs | to_entries[].value' | $(CACHIX) push spuriobot
push_shell:
	mkdir -p .nix-profiles
	$(NIX) develop --profile .nix-profiles/dev-profile -c true
	$(CACHIX) push spuriobot .nix-profiles/dev-profile
	
