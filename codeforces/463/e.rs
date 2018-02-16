#![allow(unused_imports, unused_macros, dead_code)]
use std::ops::*;
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
const NOTHING: IntMod = IntMod(MOD);
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

struct Memo {
    n: usize,
    memo: Vec<Vec<IntMod>>,
}

impl Memo {
    fn new(n: usize, k: usize) -> Self {
        Memo {
            n: n,
            memo: vec![vec![NOTHING;k+1];k+1],
        }
    }

    fn index(&self, n: usize) -> usize {
        self.n - n
    }

    fn query(&mut self, n: usize, k: usize) -> IntMod {
        let i = self.index(n);
        if self.memo[i][k] != NOTHING {
            self.memo[i][k]
        } else {
            if k == 1 {
                let res = IntMod(n as i64) * IntMod(pow_mod(2, n - 1, MOD));
                self.memo[i][k] = res;
                res
            } else {
                let res = IntMod(n as i64) * (self.query(n, k - 1) - self.query(n - 1, k - 1));
                self.memo[i][k] = res;
                res
            }
        }
    }
}

fn main() {
    let (n, k): (usize, usize) = readln();
    let mut memo: Memo = Memo::new(n, k);
    println!("{}", memo.query(n, k));
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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

impl Sub for IntMod {
    type Output = IntMod;
    fn sub(self, rhs: IntMod) -> IntMod {
        IntMod((self.0 + MOD - rhs.0) % MOD)
    }
}

impl Mul for IntMod {
    type Output = IntMod;
    fn mul(self, rhs: IntMod) -> IntMod {
        IntMod((self.0 * rhs.0) % MOD)
    }
}

impl Div for IntMod {
    type Output = IntMod;
    fn div(self, rhs: IntMod) -> IntMod {
        let x = self.0 * ext_gcd(rhs.0, MOD).0;
        IntMod(if x < 0 { x % MOD + MOD } else { x % MOD })
    }
}

impl AddAssign for IntMod {
    fn add_assign(&mut self, other: IntMod) {
        *self = *self + other
    }
}

impl SubAssign for IntMod {
    fn sub_assign(&mut self, other: IntMod) {
        *self = *self - other
    }
}

impl MulAssign for IntMod {
    fn mul_assign(&mut self, other: IntMod) {
        *self = *self * other
    }
}

impl DivAssign for IntMod {
    fn div_assign(&mut self, other: IntMod) {
        *self = *self / other
    }
}

pub fn gcd(x: i64, y: i64) -> i64 {
    let mut a = x.abs();
    let mut b = y.abs();
    while b > 0 {
        let r = a % b;
        a = b;
        b = r;
    }
    a
}

pub fn lcm(x: i64, y: i64) -> i64 {
    if x == 0 || y == 0 {
        0
    } else {
        (x / gcd(x, y) * y).abs()
    }
}

pub fn ext_gcd(x: i64, y: i64) -> (i64, i64) {
    debug_assert!(x > 0 && y > 0);
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

pub fn inv_mod(x: i64, modulus: i64) -> i64 {
    let mut r0 = x;
    let mut r1 = modulus;
    let mut x0 = 1;
    let mut x1 = 0;
    while r1 > 0 {
        let q = r0 / r1;
        r0 = r0 % r1;
        std::mem::swap(&mut r0, &mut r1);
        x0 = x0 - q * x1;
        std::mem::swap(&mut x0, &mut x1);
    }
    let res = x0 % modulus;
    if res < 0 { res + modulus } else { res }
}

pub fn pow_mod(x: i64, n: usize, modulus: i64) -> i64 {
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
