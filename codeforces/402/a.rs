#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

fn main() {
    let n = readln();
    let xs: Vec<usize> = readln();
    let ys: Vec<usize> = readln();
    let mut fx: Vec<usize> = vec![0;6];
    let mut fy: Vec<usize> = vec![0;6];
    let mut freq: Vec<usize> = vec![0;6];
    for i in 0..n {
        fx[xs[i]] += 1;
        fy[ys[i]] += 1;
        freq[xs[i]] += 1;
        freq[ys[i]] += 1;
    }

    for i in 1..6 {
        if freq[i] % 2 != 0 {
            println!("-1");
            return;
        }
    }

    let mut res = 0;
    for i in 1..6 {
        let k = freq[i] / 2;
        if fx[i] > k {
            res += fx[i] - k;
        }
    }

    println!("{}", res);

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
