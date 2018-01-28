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

struct DFS {
    n: usize,
    gr: Vec<Vec<(usize, i64)>>,
    a: Vec<i64>,
    visited: Vec<bool>,
}

impl DFS {
    fn new(gr: &Vec<Vec<(usize, i64)>>) -> DFS {
        DFS {
            n: gr.len(),
            gr: gr.clone(),
            a: vec![INF;gr.len()],
            visited: vec![false;gr.len()],
        }
    }

    fn run(&mut self, v: usize) -> bool {
        if self.visited[v] {
            return true;
        }
        self.visited[v] = true;
        if self.a[v] == INF {
            self.a[v] = 0;
        }
        for i in 0..self.gr[v].len() {
            let (u, d) = self.gr[v][i];
            if self.a[u] == INF {
                self.a[u] = self.a[v] + d;
                if !self.run(u) {
                    return false;
                }
            } else if self.a[u] != self.a[v] + d {
                return false;
            } else {
                continue;
            }
        }
        true
    }
}

fn main() {
    let (n, m): (usize, usize) = readln();
    let mut gr: Vec<Vec<(usize, i64)>> = vec![vec![];n];

    for _ in 0..m {
        let (l, r, d): (usize, usize, i64) = readln();
        gr[l - 1].push((r - 1, d));
        gr[r - 1].push((l - 1, -d));
    }

    let mut dfs = DFS::new(&gr);
    for i in 0..n {
        if !dfs.run(i) {
            println!("No");
            return;
        }
    }

    println!("Yes");

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
