#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

fn main() {
    let (n, m): (usize, usize) = readln();
    let mut freq: Vec<usize> = vec![1;n+1];
    let mut flag: Vec<bool> = vec![false;n+1];
    freq[0] = 0;
    flag[1] = true;
    for _ in 0..m {
        let (x, y): (usize, usize) = readln();

        freq[x] -= 1;
        freq[y] += 1;
        flag[y] |= flag[x];

        if freq[x] == 0 {
            flag[x] = false;
        }
    }

    let mut res = 0;
    for b in flag {
        if b {
            res += 1;
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
