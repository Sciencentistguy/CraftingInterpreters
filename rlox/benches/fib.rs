use criterion::{criterion_group, criterion_main, Criterion};
use rlox::virtual_machine::VirtualMachine;

fn fib_35() {
    const PROGRAM: &str = "
        fun fib(n) {
            if (n < 2) return n;
            return fib(n - 2) + fib(n - 1);
        }

        print fib(35) == 9227465;
    ";
    let mut vm = VirtualMachine::new();
    vm.reset(PROGRAM, 0).unwrap();
    vm.start().unwrap();
}

fn bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("fib_35");
    group.sample_size(10);
    group.bench_function("my-function", |b| b.iter(fib_35));
    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
