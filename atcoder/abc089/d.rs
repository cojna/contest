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
    let (h, w, d): (usize, usize, usize) = readln();
    let mut a: Vec<(usize, usize)> = vec![(NOTHING, NOTHING); h * w];
    for i in 0..h {
        let xs: Vec<usize> = readln();
        for j in 0..w {
            a[xs[j] - 1] = (i, j);
        }
    }

    let mut dist: Vec<i64> = vec![INF;h*w];

    for i in 0..d {
        dist[i] = 0;
        let mut px = a[i].0;
        let mut py = a[i].1;
        let mut j = i;
        while j + d < h * w {
            let x = a[j + d].0;
            let y = a[j + d].1;
            dist[j + d] = dist[j] + (x as i64 - px as i64).abs() + (y as i64 - py as i64).abs();
            j += d;
            px = x;
            py = y;
        }
    }

    let q: usize = readln();
    let mut res: Vec<i64> = Vec::with_capacity(q);
    for _ in 0..q {
        let (l, r): (usize, usize) = readln();
        res.push(dist[r - 1] - dist[l - 1]);
    }
    let strs: Vec<String> = res.iter().map(|x| x.to_string()).collect();
    println!("{}", strs.join("\n"));
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
