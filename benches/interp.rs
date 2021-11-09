use criterion::{black_box, criterion_group, criterion_main, Criterion};
use leggevm::{compile, vm, LogLevel, Timing};

pub fn compare(c: &mut Criterion) {
    c.bench_function("reference", |b| {
        b.iter(|| {
            fn add(i: isize) -> isize {
                let mut val = 0;
                if i >= 0 {
                    val = black_box(i + val + add(i - 1));
                }
                return val;
            }
            let n = 10000;
            let res = add(n);
            assert_eq!(res * 2, n * (n + 1))
        });
    });

    let mut timing = Timing::default();
    let runtime = vm::Runtime::default();
    let (bytecode, _) = compile(
        &mut timing,
        &runtime,
        LogLevel::LogNone,
        "
            import assert;
            add :: fn(i: int) -> int {
              val := 0;
              if (i >= 0) {
                val = i + val + add(i-1);
              }
              return val;
            }
            n :: 10000;
            res :: add(n);

            assert(res*2, n*(n+1));
        "
        .into(),
    )
    .unwrap();

    c.bench_function("interpreter", |b| {
        b.iter(|| {
            let mut interp = vm::Interpreter::new(&runtime, LogLevel::LogNone, &|_| {});
            interp.run(&bytecode);
        });
    });
}

criterion_group!(benches, compare);
criterion_main!(benches);
