#![allow(unused_imports, unused_macros, dead_code)]
use std::f64::*;
use std::cmp::*;
use std::ops::*;
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

const N: usize = 100;
const L: usize = 1000;

fn main() {
    let (_, _): (usize, usize) = readln();
    let a: Vec<usize> = readln();
    let b: Vec<usize> = readln();
    let memo = gen_partition();
    let mut sa = 0;
    for &x in &a {
        sa += x;
    }
    let ca = compress(&a);
    let mut sb = 0;
    for &x in &b {
        sb += x;
    }

    let cb = compress(&b);
    println!("{}", solve(&ca, sb, &memo) * solve(&cb, sa, &memo));
}

fn solve(v: &Vec<usize>, s: usize, memo: &Vec<Vec<IntMod>>) -> IntMod {

    let len = v.len();
    let mut dp: Vec<Vec<IntMod>> = vec![vec![IntMod(0);s+1];len+1];

    for i in 0..len + 1 {
        dp[i][0] = IntMod(1);
    }

    for i in 1..len + 1 {
        for j in 1..s + 1 {
            for k in 0..j + 1 {
                dp[i][j] += dp[i - 1][j - k] * memo[v[i - 1]][k];
            }
        }
    }

    dp[len][s]
}

fn sum(v: &Vec<usize>) -> usize {
    let mut res = 0;
    for &x in v {
        res += x;
    }
    res
}

fn compress(v: &Vec<usize>) -> Vec<usize> {
    let mut res: Vec<usize> = vec![];
    let mut freq: Vec<usize> = vec![0; L + 1];
    for &x in v {
        freq[x] += 1;
    }
    for &f in &freq {
        if f > 0 {
            res.push(f);
        }
    }
    res
}

fn gen_partition() -> Vec<Vec<IntMod>> {
    let mut dp = vec![vec![IntMod(0);L+1];N+1];
    dp[0][0] = IntMod(1);
    for i in 1..N + 1 {
        for j in 0..L + 1 {
            dp[i][j] = dp[i - 1][j];
            if i <= j {
                dp[i][j] += dp[i][j - i];
            }
        }
    }
    dp
}

#[derive(Debug, Clone, Copy)]
struct IntMod(i64);

impl std::fmt::Display for IntMod {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add for IntMod {
    type Output = IntMod;
    fn add(self, rhs: IntMod) -> IntMod {
        IntMod((self.0 + rhs.0) % MOD)
    }
}

impl AddAssign for IntMod {
    fn add_assign(&mut self, other: IntMod) {
        *self = *self + other
    }
}

impl Sub for IntMod {
    type Output = IntMod;
    fn sub(self, rhs: IntMod) -> IntMod {
        IntMod((self.0 + MOD - rhs.0) % MOD)
    }
}

impl SubAssign for IntMod {
    fn sub_assign(&mut self, other: IntMod) {
        *self = *self - other
    }
}

impl Mul for IntMod {
    type Output = IntMod;
    fn mul(self, rhs: IntMod) -> IntMod {
        IntMod((self.0 * rhs.0) % MOD)
    }
}

impl MulAssign for IntMod {
    fn mul_assign(&mut self, other: IntMod) {
        *self = *self * other
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
