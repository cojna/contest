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
    let mut res = 0;

    let (n, m, k): (usize, usize, usize) = readln();
    let mut c: Vec<Vec<char>> = Vec::with_capacity(n);

    for _ in 0..n {
        let s: Vec<char> = readln();
        c.push(s);
    }

    for i in 0..n {
        let mut x = 0;
        for j in 0..m {
            if c[i][j] == '*' {
                if x >= k {
                    res += x - k + 1;
                }
                x = 0;
            } else {
                x += 1;
                if j == m - 1 && x >= k {
                    res += x - k + 1;
                }
            }
        }
    }

    if k == 1 {
        println!("{}", res);
        return;
    }

    for j in 0..m {
        let mut x = 0;
        for i in 0..n {
            if c[i][j] == '*' {
                if x >= k {
                    res += x - k + 1;
                }
                x = 0;
            } else {
                x += 1;
                if i == n - 1 && x >= k {
                    res += x - k + 1;
                }
            }
        }
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
