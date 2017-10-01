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
    let (n, k): (usize, u32) = readln();
    let mut a: Vec<u32> = Vec::with_capacity(n);
    let mut v: Vec<i64> = Vec::with_capacity(n);
    for _ in 0..n {
        let (x, y) = readln();
        a.push(x);
        v.push(y);
    }

    if k == 0 {
        let mut res = 0;
        for i in 0..n {
            if a[i] == 0 {
                res += v[i];
            }
        }
        println!("{}", res);
        return;
    }

    let l = k.leading_zeros();
    let mut h_bit = 1u32 << (31 - l);
    let mut c: Vec<u32> = vec![];
    c.push(k);
    c.push(h_bit - 1);
    while h_bit > 0 {
        let x = (k ^ h_bit) | (h_bit - 1);
        h_bit = h_bit >> 1;
        if x <= k {
            c.push(x);
        }
    }
    c.sort();
    c.dedup();

    let mut res = 0i64;
    for i in 0..c.len() {
        let x = c[i];
        let mut tmp: i64 = 0i64;
        for j in 0..n {
            if x | a[j] == x {
                tmp += v[j];
            }
        }
        res = max(res, tmp);
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
