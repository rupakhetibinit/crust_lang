use criterion::{Criterion, criterion_group, criterion_main};
use crust_backend::vm::CrustVM;
pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Crust Bytecode VM", |b| {
        b.iter(|| {
            let crust = CrustVM::new();

            let input_path = "./samples/compiler/benchmark.crust";

            crust.run_file(&input_path)
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
