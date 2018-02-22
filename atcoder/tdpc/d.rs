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

const LIM2: usize = 60;
const LIM3: usize = 40;
const LIM5: usize = 30;

fn main() {
    let (n, d): (usize, i64) = readln();

    let mut freq2 = 0;
    let mut freq3 = 0;
    let mut freq5 = 0;
    let mut x = d;

    while x % 2 == 0 {
        freq2 += 1;
        x /= 2;
    }

    while x % 3 == 0 {
        freq3 += 1;
        x /= 3;
    }

    while x % 5 == 0 {
        freq5 += 1;
        x /= 5;
    }

    if x > 1 {
        println!("0.0");
        return;
    }

    let mut dp_cur: Vec<Vec<Vec<f64>>> = vec![vec![vec![0.0;freq5+1];freq3+1];freq2+1];
    let mut dp_next: Vec<Vec<Vec<f64>>> = vec![vec![vec![0.0;freq5+1];freq3+1];freq2+1];
    dp_cur[0][0][0] = 1.0;

    for _ in 1..n + 1 {
        for a in 0..freq2 + 1 {
            for b in 0..freq3 + 1 {
                for c in 0..freq5 + 1 {
                    let p = dp_cur[a][b][c] / 6.0;
                    dp_next[a][b][c] += p;
                    dp_next[min(freq2, a + 1)][b][c] += p;
                    dp_next[a][min(freq3, b + 1)][c] += p;
                    dp_next[min(freq2, a + 2)][b][c] += p;
                    dp_next[a][b][min(freq5, c + 1)] += p;
                    dp_next[min(freq2, a + 1)][min(freq3, b + 1)][c] += p;
                }
            }
        }
        std::mem::swap(&mut dp_cur, &mut dp_next);
        for a in 0..freq2 + 1 {
            for b in 0..freq3 + 1 {
                for c in 0..freq5 + 1 {
                    dp_next[a][b][c] = 0.0;
                }
            }
        }
    }

    println!("{}", dp_cur[freq2][freq3][freq5]);

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
