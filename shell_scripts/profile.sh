#!/bin/sh
cd "$(dirname "$0")"/..
set -e

browser="Firefox Developer Edition"

me=`whoami`
now=`date +%F`
hash=`git rev-parse --short HEAD`
svg=`realpath "./tmp/graph-$now-$hash.svg"`
sudo cargo flamegraph -o "$svg" -- -vv tmp/test0.bc
sudo chown "$me":staff "$svg"
open -a "$browser" "$svg"
