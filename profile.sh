#!/bin/sh
set -e

browser="Firefox Developer Edition"
flamegraphdir="../flamegraph"

mkdir -p tmp/profile
sudo dtrace -c './target/release/main' -o ./tmp/profile/out.stacks -n 'profile-997 { @[ustack()] = count(); }'
"$flamegraphdir/stackcollapse.pl" ./tmp/profile/out.stacks > ./tmp/profile/out.folded
grep main ./tmp/profile/out.folded > ./tmp/profile/out-filtered.folded
"$flamegraphdir/flamegraph.pl" ./tmp/profile/out-filtered.folded > ./tmp/profile/out.svg

svg=`realpath ./tmp/profile/out.svg`
open -a "$browser" "$svg"
