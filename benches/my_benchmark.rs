use criterion::{criterion_group, criterion_main, Criterion};
use lox::lox::Lox;
pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("SlotMap interpreter", |b| {
        b.iter(|| {
            let lox = Lox::new();

            let input = "1 + 2 + 3 * 4 * 55 * 66 * 77;".to_string();

            _ = lox.run(&input);
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
