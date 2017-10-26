#![allow(unused_imports, dead_code)]
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
    let s: Vec<char> = readln();
    let n = s.len();

    let mut cum: Vec<u32> = vec![0;n];
    fn char_bit(c: char) -> u32 {
        1 << (c as u32 - 'a' as u32)
    }

    cum[0] = char_bit(s[0]);
    for i in 1..n {
        cum[i] = cum[i - 1] ^ char_bit(s[i]);
    }

    let mut m: Vec<u32> = vec![0x3f3f3f3fu32;1<<26];
    m[cum[0] as usize] = 1;

    let mut ps: Vec<u32> = vec![0;27];
    for i in 1..27 {
        ps[i] = 1 << (i - 1);
    }

    for i in 1..n {
        let mut x = 0x3f3f3f3fu32;
        for &p in &ps {
            x = if p == cum[i] {
                1
            } else {
                min(x, m[(p ^ cum[i]) as usize] + 1)
            };
        }
        m[cum[i] as usize] = min(m[cum[i] as usize], x);
    }

    println!("{}", m[cum[n - 1] as usize]);

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
