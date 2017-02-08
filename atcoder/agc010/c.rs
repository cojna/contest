#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const NOTHING: usize = std::usize::MAX;

fn main() {
    let n = readln();
    let cs: Vec<i64> = readln();
    let mut gr: Vec<Vec<usize>> = vec![vec![];n];
    for _ in 0..n - 1 {
        let (a, b): (usize, usize) = readln();
        gr[a - 1].push(b - 1);
        gr[b - 1].push(a - 1);
    }
    let gr = gr;

    if n == 2 {
        if cs[0] == cs[1] {
            println!("YES");
        } else {
            println!("NO");
        }
        return;
    }
    let mut root = 0;
    for v in 0..n {
        if gr[v].len() > 1 {
            root = v;
            break;
        }
    }

    if let Some(0) = dfs(&gr, root, NOTHING, &cs) {
        println!("YES");
    } else {
        println!("NO");
    }
}

fn dfs(gr: &Vec<Vec<usize>>, v: usize, parent: usize, cs: &Vec<i64>) -> Option<i64> {
    let len = gr[v].len() - 1;
    if len == 0 {
        return Some(cs[v]);
    }

    let mut children: Vec<i64> = Vec::with_capacity(len);
    for &nv in &gr[v] {
        if nv == parent {
            continue;
        }
        if let Some(c) = dfs(gr, nv, v, cs) {
            children.push(c)
        } else {
            return None;
        }
    }

    let s: i64 = children.iter().fold(0, |acc, &x| acc + x);
    let m: i64 = children.iter().fold(0, |acc, &x| max(acc, x));

    let t = 2 * cs[v] - s;

    if t >= 0 && (s - t) <= 2 * min(s / 2, s - m) && s - t >= 0 {
        Some(t)
    } else {
        None
    }

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
