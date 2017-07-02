#![allow(unused_imports, dead_code)]
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
    let (n, m): (usize, usize) = readln();
    let a: Vec<i64> = readln();
    let a: Vec<i64> = a.iter().map(|x| x - 1).collect();
    let mut diff0: Vec<i64> = vec![0;2 * m];
    let mut diff1: Vec<i64> = vec![0;2 * m];

    let mut base = 0;
    for i in 1..n {
        let d = (a[i] - a[i - 1] + m as i64) % m as i64;
        base += d;
        let l = a[i - 1] as usize + 1;
        let r = a[i - 1] as usize + d as usize + 1;
        diff1[l] -= 1;
        diff1[r] += 1;
        diff0[l] += a[i - 1] + 1;
        diff0[r] -= a[i - 1] + 1;
    }

    let mut cum0: Vec<i64> = vec![0;2 * m];
    let mut cum1: Vec<i64> = vec![0;2 * m];
    cum0[0] = diff0[0];
    cum1[1] = diff1[0];
    for i in 1..2 * m {
        cum1[i] = cum1[i - 1] + diff1[i];
        cum0[i] = cum0[i - 1] + diff0[i];
    }

    let mut res = base;
    for i in 0..m {
        let improvement = cum1[i] * (i as i64) as i64 + cum0[i] + cum1[i + m] * (i + m) as i64 +
                          cum0[i + m];
        res = min(res, base + improvement);
    }

    println!("{}", res);

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
