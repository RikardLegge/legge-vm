#!/bin/bash
cd "$(dirname "$0")"
set -e

mkdir -p ../tmp
rm -rf ../tmp/*

count=300;

for i in `seq 0 $count`; do
  ii=$((i+1));
  echo "count :: $i;" > "../tmp/test$i.bc"
  for j in `seq 0 100`; do
    i1=$(($RANDOM % $count))
    i2=$(($RANDOM % $count))
    i3=$(($RANDOM % $count))
    echo "
    if(true) {
      import std.touch;
      import .test$ii.count;
      import .test$i1.count;
      import .test$i2.count;
      import .test$i3.count;

      A -> type {
         value: int
      }
      B -> type {
         a: A
      }
      factory :: fn (new_a: Fn(int) -> A) -> Fn(int) -> B {
          return fn(value: int) -> B {
              b := B();
              b.a = new_a(value);
              return b;
          };
      }
      new :: factory(fn(v: int) -> A {
          a := A();
          a.value = v;
          return a;
      });
      touch(new(42));
      loop {
        a :: 1;
        b := 2;
        b = 2+1;
        new(b);
        break;
      }
    }
    " >> "../tmp/test$i.bc"
  done
done

echo "
count :: $ii;
" > "../tmp/test$ii.bc"
