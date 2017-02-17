#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const NOTHING: usize = std::usize::MAX;

fn main() {
    let (n, lim): (usize, i64) = readln();
    let a: Vec<i64> = readln();

    let mut k = NOTHING;
    for i in 0..n - 1 {
        if a[i] + a[i + 1] >= lim {
            k = i;
            break;
        }
    }

    let mut res = Vec::with_capacity(n - 1);
    if k == NOTHING {
        println!("Impossible")
    } else {
        for i in 0..k {
            res.push(i);
        }
        for i in (k + 1..n).rev() {
            res.push(i - 1);
        }
        let strs: Vec<String> = res.iter().map(|x| (x + 1).to_string()).collect();
        println!("Possible\n{}", strs.join("\n"));
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
