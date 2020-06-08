#! /usr/bin/env bash
set -eufo pipefail
nix-shell --run 'ghcide --lsp'