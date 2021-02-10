use criterion::{black_box, criterion_group, criterion_main, Criterion};
use leggevm::{compile, interpreter, runtime, LogLevel, Timing};

pub fn compare(c: &mut Criterion) {
    c.bench_function("reference", |b| {
        b.iter(|| {
            let mut i = 100000;
            loop {
                if i == 0 {
                    break;
                }
                i = black_box(i - 1);
            }
        });
    });

    let mut timing = Timing::default();
    let runtime = runtime::std();
    let bytecode = compile(
        &mut timing,
        &runtime,
        LogLevel::LogNone,
        "
            i := 100000;
            loop {
                if(i == 0) {
                    break;
                }
                i = i - 1;
            }
        "
        .into(),
    );

    c.bench_function("interpreter", |b| {
        b.iter(|| {
            let interp = interpreter::Interpreter::new(&runtime);
            interp.run(&bytecode);
        });
    });
}

criterion_group!(benches, compare);
criterion_main!(benches);
