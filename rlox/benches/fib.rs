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

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 35", |b| b.iter(fib_35));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
