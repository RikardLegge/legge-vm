use criterion::{black_box, criterion_group, criterion_main, Criterion};
use leggevm::interpreter::Interpreter;
use leggevm::{compile, debug, runtime};

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

    let mut timing = debug::Timing::default();
    let runtime = runtime::get();
    let bytecode = compile(
        &mut timing,
        false,
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
            let interp = Interpreter::new(&runtime);
            interp.run(&bytecode);
        });
    });
}

criterion_group!(benches, compare);
criterion_main!(benches);
