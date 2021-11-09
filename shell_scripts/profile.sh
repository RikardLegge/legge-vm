#!/bin/sh
cd "$(dirname "$0")"
set -e

browser="Firefox Developer Edition"
flamegraphdir="../../flamegraph"

mkdir -p tmp/profile
sudo rm -rf ./tmp/profile/*
cd tmp
sudo dtrace -c '../../target/release/main test0.bc' -o ./profile/out.stacks -n 'profile-997 { @[ustack()] = count(); }'
cd ..
"$flamegraphdir/stackcollapse.pl" ./tmp/profile/out.stacks > ./tmp/profile/out.folded
grep 'main`main::main' ./tmp/profile/out.folded > ./tmp/profile/out-filtered.folded
sed 's/^.*\(main`main::main.*\)$/\1/' ./tmp/profile/out-filtered.folded >./tmp/profile/out-short.folded
"$flamegraphdir/flamegraph.pl" ./tmp/profile/out-filtered.folded > ./tmp/profile/out.svg

svg=`realpath ./tmp/profile/out.svg`
open -a "$browser" "$svg"
