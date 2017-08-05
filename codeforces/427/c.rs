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

const L: usize = 100;

fn main() {
    let (n, q, c): (usize, usize, usize) = readln();
    let mut m: Vec<Vec<Vec<i64>>> = vec![vec![vec![0;L+1];L+1];c+1];
    for _ in 0..n {
        let (x, y, s): (usize, usize, usize) = readln();
        m[s][x][y] += 1;
    }

    for s in 0..c + 1 {
        for i in 0..L + 1 {
            for j in 1..L + 1 {
                m[s][i][j] += m[s][i][j - 1];
            }
        }
    }

    for s in 0..c + 1 {
        for i in 1..L + 1 {
            for j in 0..L + 1 {
                m[s][i][j] += m[s][i - 1][j];
            }
        }
    }

    let mut res: Vec<i64> = Vec::with_capacity(q);
    for _ in 0..q {
        let (t, x1, y1, x2, y2): (usize, usize, usize, usize, usize) = readln();
        let mut b: i64 = 0;
        for i in 0..c + 1 {
            let r = (i + t) % (c + 1);
            let freq = m[i][x1 - 1][y1 - 1] - m[i][x1 - 1][y2] - m[i][x2][y1 - 1] + m[i][x2][y2];
            b += r as i64 * freq;
        }
        res.push(b);
    }
    let ress: Vec<String> = res.iter().map(|i| i.to_string()).collect();

    println!("{}", ress.join("\n"));
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

impl<A: Read, B: Read, C: Read, D: Read, E: Read> Read for (A, B, C, D, E) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]),
         B::read(tokens[1]),
         C::read(tokens[2]),
         D::read(tokens[3]),
         E::read(tokens[4]))
    }
}
