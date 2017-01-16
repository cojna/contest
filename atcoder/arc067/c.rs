#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const MOD: i64 = 1_000_000_007;

fn main() {
    let n: usize = readln();
    let p = Primes::new();
    let mut freq = vec![0;n+1];
    for i in 2..n + 1 {
        for p in p.factors(i as i64) {
            freq[p as usize] += 1;
        }
    }

    let mut res = 1;
    for l in freq {
        res = res * (l as i64 + 1) % MOD;
    }

    println!("{}", res);

}

fn group<T: Eq + Copy>(vec: Vec<T>) -> Vec<Vec<T>> {
    let n = vec.len();
    let mut res = Vec::new();
    let mut i = 0;
    while i < n {
        let mut v = Vec::new();
        let x = vec[i];
        v.push(x);
        i += 1;
        while i < n {
            if vec[i] == x {
                v.push(x);
                i += 1;
            } else {
                break;
            }
        }
        res.push(v);
    }
    res
}

pub struct Primes {
    pub small_primes: Vec<i64>,
}

impl Primes {
    fn new() -> Primes {
        let mut primes: Vec<i64> = Vec::new();
        primes.push(2);
        for i in 3..Primes::int_sqrt(0x7fff_ffff) as i64 {
            let mut j = 0;
            loop {
                if primes[j] * primes[j] <= i {
                    if i % primes[j] == 0 {
                        break;
                    }
                } else {
                    primes.push(i);
                    break;
                }
                j += 1;
            }
        }
        Primes { small_primes: primes }
    }

    fn factors(&self, n: i64) -> Vec<i64> {
        debug_assert!(n <= 0x7fff_ffff);
        let mut res: Vec<i64> = Vec::new();
        let mut x = n;
        for &p in &self.small_primes {
            if x < p * p {
                if x > 1 {
                    res.push(x);
                }
                return res;
            }
            while x % p == 0 {
                res.push(p);
                x = x / p;
            }
        }
        res.push(x);
        res
    }

    fn is_prime(&self, x: i64) -> bool {
        self.factors(x).len() == 1
    }

    fn int_sqrt(x: usize) -> usize {
        f64::sqrt(x as f64) as usize
    }
}

#[allow(dead_code)]
fn get_line() -> String {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_string()
}

#[allow(dead_code)]
fn get_chars() -> Vec<char> {
    get_line().chars().collect()
}

#[allow(dead_code)]
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
