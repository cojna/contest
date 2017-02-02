#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const NOTHING: usize = std::usize::MAX;

fn main() {
    let n: usize = readln();
    let mut gr: Vec<Vec<usize>> = vec![vec![];n];
    for _ in 0..n - 1 {
        let (u, v): (usize, usize) = readln();
        gr[u - 1].push(v - 1);
        gr[v - 1].push(u - 1);
    }
    let cs: Vec<i64> = readln();

    let mut freq = vec![0;n];
    for u in 0..n {
        for &v in &gr[u] {
            if cs[u] != cs[v] {
                freq[u] += 1;
            }
        }
    }

    let mut acc = 0;
    let mut root = 0;
    for i in 0..n {
        if acc < freq[i] {
            acc = freq[i];
            root = i;
        }
    }

    for &v in &gr[root] {
        if dfs(&gr, v, root, cs[v], &cs) {
            continue;
        } else {
            println!("NO");
            return;
        }
    }

    println!("YES\n{}", root + 1);
}

fn dfs(gr: &Vec<Vec<usize>>, v: usize, parent: usize, color: i64, cs: &Vec<i64>) -> bool {
    for &u in &gr[v] {
        if u == parent {
            continue;
        }

        if color != cs[u] {
            return false;
        } else {
            if !dfs(gr, u, v, color, cs) {
                return false;
            }
        }
    }
    true
}

fn get_line() -> String {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_string()
}

fn get_chars() -> Vec<char> {
    get_line().chars().collect()
}

fn readln<T: Read>() -> T {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    T::read(buf.trim_right())
}

trait Read {
    fn read(s: &str) -> Self;
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
