use criterion::{Criterion, criterion_group, criterion_main};
use crust_backend::Engine;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Crust register VM", |b| {
        let engine = Engine::new();
        let input_path = "./samples/compiler/benchmark.crust";
        b.iter(|| engine.run_file(input_path));
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
