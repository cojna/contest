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

fn main() {
    let n: usize = readln();
    let xs: Vec<usize> = readln();
    let cs: Vec<usize> = readln();
    let vs: Vec<usize> = readln();

    let mut x_sum: Vec<usize> = vec![0;n + 1];
    let mut c_sum: Vec<usize> = vec![0;1 << n];
    let mut v_sum: Vec<usize> = vec![0;1 << n];

    for i in 0..n {
        x_sum[i + 1] = x_sum[i] + xs[i];
    }

    for bs in 0..1 << n {
        for i in 0..n {
            if bs.test_bit(i) {
                v_sum[bs] += vs[i];
                c_sum[bs] += cs[i];
            }
        }
    }

    let mut v_max: Vec<Vec<usize>> = vec![vec![0;1<<n];n+1];
    for i in 1..n {
        for bs in 0..1 << n {
            if c_sum[bs] <= x_sum[i] {
                v_max[i][bs] = v_sum[bs];
            } else {
                for j in 0..n {
                    v_max[i][bs] = max(v_max[i][bs], v_max[i][bs.clear_bit(j)]);
                }
            }
        }
    }

    let mut dp: Vec<usize> = vec![0;1<<n];
    for ms in 1..1 << n {
        let mut min_v_max = std::usize::MAX;
        for i in 0..n {
            if ms.test_bit(i) {
                min_v_max = min(min_v_max, dp[ms.clear_bit(i)]);
            }
        }
        dp[ms] = max(min_v_max, v_max[n - ms.count_ones() as usize][ms]);
    }
    println!("{}", dp[(1 << n) - 1]);
}

trait BitUtil {
    fn test_bit(self, i: usize) -> bool;

    fn set_bit(self, i: usize) -> Self;

    fn clear_bit(self, i: usize) -> Self;

    fn bit(i: usize) -> Self;

    fn bit_mask(i: usize) -> Self;
}

impl BitUtil for usize {
    #[inline(always)]
    fn test_bit(self, i: usize) -> bool {
        self & 1 << i != 0
    }
    #[inline(always)]
    fn set_bit(self, i: usize) -> Self {
        self | 1 << i
    }
    #[inline(always)]
    fn clear_bit(self, i: usize) -> Self {
        self & !(1 << i)
    }
    #[inline(always)]
    fn bit(i: usize) -> Self {
        1 << i
    }
    #[inline(always)]
    fn bit_mask(i: usize) -> Self {
        (1 << i) - 1
    }
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
