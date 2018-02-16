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
const N: usize = 1000000;

fn f(n: usize) -> usize {
    let mut res = 1;
    let mut x = n;
    while x > 1 {
        let r = x % 10;
        if r > 0 {
            res *= r;
        }
        x /= 10;
    }
    res
}

fn g(n: usize, memo: &mut Vec<usize>) -> usize {
    if memo[n] != NOTHING {
        memo[n]
    } else {
        let res = g(f(n), memo);
        memo[n] = res;
        res
    }
}
fn main() {
    let mut table: Vec<Vec<usize>> = vec![vec![0;10];N+1];
    let mut memo: Vec<usize> = vec![NOTHING;N+1];
    for i in 1..10 {
        table[i][i] += 1;
        memo[i] = i;
    }
    for i in 10..N + 1 {
        table[i][g(i, &mut memo)] += 1;
    }
    for i in 1..N + 1 {
        for j in 0..10 {
            table[i][j] += table[i - 1][j];
        }
    }


    let q: usize = readln();
    let mut ress: Vec<usize> = Vec::with_capacity(q);
    for _ in 0..q {
        let (l, r, k): (usize, usize, usize) = readln();
        ress.push(table[r][k] - table[l - 1][k]);
    }
    let res: Vec<String> = ress.iter().map(|x| x.to_string()).collect();
    println!("{}", res.join("\n"));
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
