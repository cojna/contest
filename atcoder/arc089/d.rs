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
    let (n, k): (usize, usize) = readln();
    let mut ws: Vec<Vec<usize>> = vec![vec![0;2 * k]; 2 * k];
    let mut bs: Vec<Vec<usize>> = vec![vec![0;2 * k]; 2 * k];
    let mut b: usize = 0;
    let mut w: usize = 0;

    for _ in 0..n {
        let (x, y, s): (usize, usize, String) = readln();
        if s == "W" {
            ws[x % (2 * k)][y % (2 * k)] += 1;
            w += 1;
        } else {
            bs[x % (2 * k)][y % (2 * k)] += 1;
            b += 1;
        }
    }

    for i in 0..2 * k {
        for j in 0..2 * k {
            if i > 0 {
                ws[i][j] += ws[i - 1][j];
                bs[i][j] += bs[i - 1][j];
            }

            if j > 0 {
                ws[i][j] += ws[i][j - 1];
                bs[i][j] += bs[i][j - 1];
            }
            if i > 0 && j > 0 {
                ws[i][j] -= ws[i - 1][j - 1];
                bs[i][j] -= bs[i - 1][j - 1];
            }
        }
    }

    fn sum(m: &Vec<Vec<usize>>, x0: usize, y0: usize, x1: usize, y1: usize) -> usize {
        let mut res = 0;
        if x1 > 0 && y1 > 0 {
            res += m[x1 - 1][y1 - 1]
        };
        if x0 > 0 && y0 > 0 {
            res += m[x0 - 1][y0 - 1];
        }
        if x0 > 0 && y1 > 0 {
            res -= m[x0 - 1][y1 - 1];
        }
        if y0 > 0 && x1 > 0 {
            res -= m[x1 - 1][y0 - 1];
        }
        res
    }

    fn query(m: &Vec<Vec<usize>>, rx: usize, ry: usize, k: usize) -> usize {
        let mut res = 0;
        res += sum(m, 0, 0, rx, ry);
        res += sum(m, 0, ry + k, rx, 2 * k);
        res += sum(m, rx, ry, rx + k, ry + k);
        res += sum(m, rx + k, 0, 2 * k, ry);
        res += sum(m, rx + k, ry + k, 2 * k, 2 * k);
        res
    }
    let mut res: usize = 0;
    for rx in 0..k {
        for ry in 0..k {
            let res_w = query(&ws, rx, ry, k);
            let res_b = query(&bs, rx, ry, k);
            res = max(res, res_w + b - res_b);
            res = max(res, res_b + w - res_w);
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
