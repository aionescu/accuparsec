#!/bin/sh
mkdir -p gcl/bench/results
cabal run -v0 gcl:bench -- --json gcl/bench/results/gcl_$(date "+%Y-%m-%d_%H:%M:%S").json
