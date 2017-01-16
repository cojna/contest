#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const MOD: i64 = 1_000_000_007;

fn main() {
    let is: Vec<usize> = readln();
    let n = is[0];
    let a = is[1];
    let b = is[2];
    let c = is[3];
    let d = is[4];

    let mut dp: Vec<Vec<i64>> = vec![vec![0;n+1]; n + 1];
    let mut fact: Vec<i64> = vec![1;n+1];
    for i in 1..n + 1 {
        fact[i] = fact[i - 1] * (i as i64) % MOD;
    }

    for i in 0..n + 1 {
        dp[i][0] = 1;
    }

    for p in a..b + 1 {
        for i in a..n + 1 {
            dp[p][i] = dp[p - 1][i];
            for g in c..d + 1 {
                if i < p * g {
                    break;
                }
                let mut x = dp[p - 1][i - p * g];
                let r = n - (i - p * g);
                x = x * fact[r] % MOD;
                let mut y = pow_mod(fact[p], g, MOD);
                y = y * fact[r - p * g] % MOD;
                y = y * fact[g] % MOD;
                let x = x * inv_mod(y, MOD) % MOD;

                dp[p][i] = (dp[p][i] + x) % MOD;
            }
        }
    }

    println!("{}", dp[b][n]);
}

fn ext_gcd(x: i64, y: i64) -> (i64, i64) {
    let mut r0 = x;
    let mut r1 = y;
    let mut x0 = 1;
    let mut y0 = 0;
    let mut x1 = 0;
    let mut y1 = 1;
    while r1 > 0 {
        let q = r0 / r1;
        r0 = r0 % r1;
        std::mem::swap(&mut r0, &mut r1);
        x0 = x0 - q * x1;
        std::mem::swap(&mut x0, &mut x1);
        y0 = y0 - q * y1;
        std::mem::swap(&mut y0, &mut y1);
    }
    (x0, y0)
}

fn inv_mod(x: i64, modulus: i64) -> i64 {
    let res = ext_gcd(x, modulus).0;
    if res < 0 {
        res % modulus + modulus
    } else {
        res % modulus
    }
}

fn pow_mod(x: i64, n: usize, modulus: i64) -> i64 {
    let mut res = 1;
    let mut x = x;
    let mut n = n;
    while n > 0 {
        if n & 1 != 0 {
            res = res * x % modulus;
        }
        x = x * x % modulus;
        n = n >> 1;
    }
    res
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
