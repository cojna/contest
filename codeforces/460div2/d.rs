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
    let (n, m): (usize, usize) = readln();
    let cs: Vec<char> = readln();
    let label: Vec<usize> = cs.iter().map(|&c| c as usize - 'a' as usize).collect();
    let mut gr: Vec<Vec<usize>> = vec![vec![];n];

    for _ in 0..m {
        let (x, y): (usize, usize) = readln();
        gr[x - 1].push(y - 1);
    }
    let mut res = 0;
    if let Some(top) = topological_sort(&gr) {
        let mut dp: Vec<Vec<i64>> = vec![vec![0;26];n];
        for &v in &top {
            dp[v][label[v]] += 1;
            for j in 0..26 {
                res = max(res, dp[v][j]);
                for &nv in &gr[v] {
                    dp[nv][j] = max(dp[nv][j], dp[v][j]);
                }
            }
        }
        println!("{}", res);
    } else {
        println!("-1");
    }

}

type VertexId = usize;

pub fn topological_sort(gr: &Vec<Vec<VertexId>>) -> Option<Vec<VertexId>> {
    let n = gr.len();
    let mut top: Vec<VertexId> = Vec::with_capacity(n);
    let mut in_deg: Vec<usize> = vec![0;n];
    let mut l: usize = 0;
    let mut r: usize = 0;

    for v in 0..n {
        for &nv in &gr[v] {
            in_deg[nv] += 1;
        }
    }

    for v in 0..n {
        if in_deg[v] == 0 {
            top.push(v);
            r += 1;
        }
    }

    while l < r {
        for &v in &gr[top[l]] {
            in_deg[v] -= 1;
            if in_deg[v] == 0 {
                top.push(v);
                r += 1;
            }

        }
        l += 1;
    }

    if top.len() == n { Some(top) } else { None }
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
