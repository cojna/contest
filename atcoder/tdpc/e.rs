#![allow(unused_imports, unused_macros, dead_code)]
use std::f64::*;
use std::cmp::*;
use std::collections::*;

macro_rules! dump{
    ($($a:expr),*) => {
        println!(concat!($(stringify!($a), " = {:?}, "),*), $($a),*);
    }
}

const INF: i64 = 0x3f3f3f3f3f3f3f3f;
const MOD: i64 = 1000000007;
const NOTHING: usize = std::usize::MAX;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

fn main() {
    let d: usize = readln();
    let s: Vec<char> = readln();
    let xs: Vec<usize> = s.iter().map(|&c| c as usize - '0' as usize).collect();
    let mut dp0: Vec<Vec<i64>> = vec![vec![0;2];d];
    let mut dp1: Vec<Vec<i64>> = vec![vec![0;2];d];

    dp0[0][0] = 1;
    for &x in &xs {
        for i in 0..d {
            dp1[(i + x) % d][0] += dp0[i][0];
            dp1[(i + x) % d][0] %= MOD;
            for j in 0..x {
                dp1[(i + j) % d][1] += dp0[i][0];
                dp1[(i + j) % d][1] %= MOD;
            }
            for j in 0..10 {
                dp1[(i + j) % d][1] += dp0[i][1];
                dp1[(i + j) % d][1] %= MOD;
            }
        }
        std::mem::swap(&mut dp0, &mut dp1);
        for i in 0..d {
            for j in 0..2 {
                dp1[i][j] = 0;
            }
        }
    }
    println!("{}", (dp0[0][0] + dp0[0][1] - 1) % MOD);
}

trait Read {
    fn read(s: &str) -> Self;
}

fn readln<T: Read>() -> T {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    T::read(buf.trim_right())
}

macro_rules! read_impl{
    ($($t:ty)*) => ($(
        impl Read for $t {
            fn read(s: &str) -> $t{
                s.parse().unwrap()
            }
        }
    )*)
}
read_impl! { usize u8 u16 u32 u64 isize i8 i16 i32 i64 f32 f64 }

impl Read for String {
    fn read(s: &str) -> Self {
        s.to_string()
    }
}

impl Read for Vec<char> {
    fn read(s: &str) -> Self {
        s.chars().collect()
    }
}

impl<T: Read> Read for Vec<T> {
    fn read(s: &str) -> Self {
        s.split_whitespace().map(T::read).collect()
    }
}

impl<A: Read, B: Read> Read for (A, B) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]))
    }
}

impl<A: Read, B: Read, C: Read> Read for (A, B, C) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]))
    }
}

impl<A: Read, B: Read, C: Read, D: Read> Read for (A, B, C, D) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]), D::read(tokens[3]))
    }
}
