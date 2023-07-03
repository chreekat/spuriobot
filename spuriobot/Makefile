.PHONY: test build push push_inputs push_outputs push_shell db database db.service db.cluster db.isrunning db.stop db.clean

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


##
## Dev Postgres environment
##

export PGDATA ?= $(CURDIR)/.postgres-work
export PGDATABASE ?= ci_failure
export PGHOST := $(PGDATA)

pg_isready := pg_isready -q
pg_ctl := $(shell pg_config --bindir)/pg_ctl

# Start postgres process, put all the files in the same place, and wait for
# startup to complete before continuting.
# pg_ctl: -w: wait, -o: postgres options (need quotes), -l: log file path
# postgres: -F: no fsync, -h '': no IP/hostname connections. -k: socket dir
pg_start := $(pg_ctl) \
    start -w -o "-F -h '' -k $(PGHOST)" -l $(PGDATA)/logfile

# Example usage:  $(db_exists) foo
#
# Result is returned as exit status.
#   - https://stackoverflow.com/a/16783253/994643
db_exists := psql -lqt | cut -f1 -d \| | grep -qw


db: database

database: db.service ; $(db_exists) $(PGDATABASE) || createdb $(PGDATABASE)

db.service: db.cluster ; $(pg_isready) || $(pg_start)

# This is one of the files created when a cluster is initialized.
cluster_sentinel := $(PGDATA)/PG_VERSION
db.cluster: $(cluster_sentinel)

$(cluster_sentinel): 
	mkdir -p $(PGDATA)
	$(pg_ctl) initdb -D $(PGDATA)

db.isrunning: ; $(pg_isready)
db.stop: ; -$(pg_isready) && $(pg_ctl) stop
db.clean: stop ; rm -rf $(PGDATA)
