#!/bin/sh
mkdir -p gcl/bench/results
out=gcl/bench/results/gcl_$(date "+%Y-%m-%d_%H:%M:%S").json
cabal run -v0 gcl:bench -- --json $out
python3 gcl/bench/plot.py $out
