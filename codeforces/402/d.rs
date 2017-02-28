#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

fn main() {
    let t = get_chars();
    let p = get_chars();
    let a: Vec<usize> = readln();
    let a: Vec<usize> = a.iter().map(|x| x - 1).collect();
    let mut low = 0;
    let mut high = a.len();

    while high - low > 1 {
        let mid = low + ((high - low) >> 1);
        let mut invalid = HashSet::with_capacity(mid);
        for i in 0..mid {
            invalid.insert(a[i]);
        }

        let mut i = 0;
        let mut j = 0;
        while i < t.len() && j < p.len() {
            if t[i] == p[j] && !invalid.contains(&i) {
                i += 1;
                j += 1;
            } else {
                i += 1;
            }
        }

        if j == p.len() {
            low = mid;
        } else {
            high = mid;
        }
    }

    println!("{}", low);
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
