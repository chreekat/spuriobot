# About Spuriobot

This is a service for identifying, recording, and recovering from spurious
failures in CI. Currently it is specialized for GHC on
https://gitlab.haskell.org.

A public dashboard powered by Spuriobot's results is found at
https://grafana.gitlab.haskell.org/d/167r9v6nk/ci-spurious-failures?orgId=2&refresh=15m.

Architecturally, Spuriobot is a web service that listens to GitLab webhooks. It
identifies spurious failures by searching for known strings in the build logs.

# Spuriobot development

## Building

Known to work:

* GHC 9.4.8 (Use GHCup to get GHC) + Cabal
* Nix + Cabal

Will probably work:

* Stack


## Using the local test database

Create and start a directory-local Postgres cluster with `make db`. This will
also create a database.

To connect to the database, specify `PGHOST=$PWD/.postgres-work`. E.g. to
connect with psql, use `PGHOST=$PWD/.postgres-work psql ci_failure`

Stop the cluster with `make db.stop`. Remove it with `make db.clean`.
