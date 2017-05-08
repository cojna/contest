#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const NOTHING: usize = std::usize::MAX;

fn main() {
    let (n, m): (usize, usize) = readln();
    let mut gr: Vec<Vec<(usize, usize)>> = vec![vec![];n];
    let mut src: Vec<usize> = vec![NOTHING;m];
    let mut dst: Vec<usize> = vec![NOTHING;m];
    let mut used: Vec<bool> = vec![false;m];
    for i in 0..m {
        let (a, b): (usize, usize) = readln();
        let a = a - 1;
        let b = b - 1;
        src[i] = a;
        dst[i] = b;
        gr[a].push((b, i));
        gr[b].push((a, i));
    }

    fn dfs(gr: &Vec<Vec<(usize, usize)>>, used: &mut Vec<bool>, cur: usize, goal: usize) -> bool {
        if cur == goal {
            return true;
        } else {
            for &(v, i) in &gr[cur] {
                if used[i] {
                    continue;
                } else {
                    used[i] = true;
                    return dfs(gr, used, v, goal);
                }
            }
            return false;
        }
    }

    for i in 0..m {
        if used[i] {
            continue;
        }
        used[i] = true;
        if dfs(&gr, &mut used, src[i], dst[i]) {
            continue;
        } else {
            println!("NO");
            return;
        }
    }
    println!("YES");

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
