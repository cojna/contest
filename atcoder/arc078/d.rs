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
    let n: usize = readln();
    let mut gr: Vec<Vec<usize>> = vec![vec![];n];
    for _ in 0..n - 1 {
        let (a, b): (usize, usize) = readln();
        gr[a - 1].push(b - 1);
        gr[b - 1].push(a - 1);
    }
    let mut q: VecDeque<(usize, usize, usize)> = VecDeque::new();
    let fennec = 0;
    let snuke = n - 1;
    q.push_back((fennec, NOTHING, 0));
    let mut depth = NOTHING;
    let mut parent: Vec<usize> = vec![NOTHING;n];
    while let Some((v, p, d)) = q.pop_front() {
        if v == snuke {
            depth = d;
            break;
        }
        for &u in &gr[v] {
            if u != p {
                q.push_back((u, v, d + 1));
                parent[u] = v;
            }
        }
    }
    let mut d = (depth - 1) / 2;
    let mut r = snuke;
    while d > 0 {
        r = parent[r];
        d -= 1;
    }

    let mut count = 0;
    let mut q: VecDeque<(usize, usize)> = VecDeque::new();
    q.push_back((r, parent[r]));
    while let Some((v, p)) = q.pop_front() {
        for &u in &gr[v] {
            if u != p {
                count += 1;
                q.push_back((u, v));
            }
        }
    }
    if count * 2 < n - 2 {
        println!("Fennec");
    } else {
        println!("Snuke");
    }
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
