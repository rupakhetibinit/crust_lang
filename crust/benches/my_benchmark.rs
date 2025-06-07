use criterion::{Criterion, criterion_group, criterion_main};
use interpreter::crust::Crust;
pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Crust Interpreter", |b| {
        b.iter(|| {
            let mut crust = Crust::new();

            let input = "1 + 2 + 3 * 4 * 55 * 66 * 77;".to_string();

            crust.run_line(&input);
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
