use criterion::{black_box, criterion_group, criterion_main, Criterion};
use node_semver::{Range, Version};

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("range", |b| {
        b.iter(|| {
            let range = Range::parse(black_box(">=1.2.3-rc.4")).unwrap();
            let version = Version::parse(black_box("1.2.3")).unwrap();

            let _r = range.satisfies(black_box(&version));
        })
    });
}

criterion_group!(bench, criterion_benchmark);
criterion_main!(bench);
