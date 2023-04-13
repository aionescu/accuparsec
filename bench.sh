#!/bin/sh
set -euo pipefail

mkdir -p bench-results
out=bench-results/bench_$(date "+%Y-%m-%d_%H:%M:%S").json

cabal bench -v0 -- --json $out
