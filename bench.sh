#!/bin/sh
set -euo pipefail

mkdir -p bench-results
out=bench-results/bench_$(date "+%Y-%m-%d_%H:%M:%S").json

cabal -v0 run bench -- --json "$out"

python3 plot/plot.py "$out"
