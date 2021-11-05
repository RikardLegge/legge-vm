#!/bin/sh
set -e

for i in `seq 0 300`; do
  ii=$((i+1));
  echo "count :: $i;" > "tmp/test$i.bc"
  for j in `seq 0 100`; do
    echo "
    if(true) {
      import std.touch;
      import local.test$ii.count;

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
    " >> "tmp/test$i.bc"
  done
done

echo "
count :: $ii;
" > "tmp/test$ii.bc"
