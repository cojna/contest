#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const INF: i64 = 0x3f3f3f3f3f3f3f3f;
const NOTHING: usize = std::usize::MAX;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

fn main() {
    let (n, k, q): (usize, i64, usize) = readln();
    let mut diff: Vec<i64> = vec![0;200010];
    let mut cum: Vec<i64> = vec![0;200010];
    let mut a: Vec<i64> = vec![0;200010];
    let mut s: Vec<i64> = vec![0;200010];

    for _ in 0..n {
        let (l, r): (usize, usize) = readln();
        diff[l - 1] += 1;
        diff[r] -= 1;
    }

    cum[0] = diff[0];
    if cum[0] >= k {
        a[0] = 1;
        s[0] = 1;
    }
    for i in 1..200000 {
        cum[i] = cum[i - 1] + diff[i];
        if cum[i] >= k {
            a[i] = 1;
            s[i] = s[i - 1] + 1;
        } else {
            s[i] = s[i - 1];
        }
    }

    let mut res: Vec<i64> = Vec::with_capacity(q);
    for _ in 0..q {
        let (a, b): (usize, usize) = readln();
        let a = a - 1;
        let b = b - 1;
        if a == 0 {
            res.push(s[b]);
        } else {
            res.push(s[b] - s[a - 1]);
        }
    }

    let strs: Vec<String> = res.iter().map(|x| x.to_string()).collect();
    println!("{}", strs.join(" "));
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

impl<A: Read, B: Read, C: Read, D: Read> Read for (A, B, C, D) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]), D::read(tokens[3]))
    }
}
