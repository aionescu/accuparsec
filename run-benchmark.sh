#!/bin/sh
cabal run gcl:bench -- --json gcl/bench/results/gcl_$(date "+%Y-%m-%d_%H:%M:%S").json
