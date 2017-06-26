#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const INF: i64 = 0x3f3f3f3f3f3f3f3f;
const MOD: i64 = 1000000007;
const NOTHING: usize = std::usize::MAX;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

macro_rules! dump{
    ($($a:expr),*) => {
        println!(concat!($(stringify!($a), " = {:?}, "),*), $($a),*);
    }
}

fn main() {
    let (n, k): (usize, i64) = readln();
    let mut cur: IntVec = vec![0; 16];

    cur[0] = 1;
    for _ in 0..n {
        let (a, b, c): (i64, i64, usize) = readln();
        let b = min(b, k);

        let mut unit: IntMat = vec![vec![0;16];16];
        for i in 0..c + 1 {
            if i > 0 {
                unit[i][i - 1] = 1;
            }
            unit[i][i] = 1;
            if i + 1 <= c {
                unit[i][i + 1] = 1;
            }
        }

        cur = pow_mul_vec(&unit, b - a, &cur);
    }

    println!("{}", cur[0]);
}

type IntMat = Vec<Vec<i64>>;
type IntVec = Vec<i64>;

fn pow_mul_vec(mat: &IntMat, k: i64, vec: &IntVec) -> IntVec {
    let mut res = vec.clone();
    let mut mat = mat.clone();
    let mut k = k;

    while k > 0 {
        if k & 1 == 1 {
            res = mul_vec(&mat, &res);
        }
        mat = mul_mat(&mat, &mat);
        k = k >> 1;
    }

    res
}

fn mul_mat(a: &IntMat, b: &IntMat) -> IntMat {
    let l = a.len();
    let m = b.len();
    let n = b[0].len();
    let mut res = vec![vec![0;n];l];

    for i in 0..l {
        for j in 0..n {
            for k in 0..m {
                res[i][j] = (res[i][j] + a[i][k] * b[k][j] % MOD) % MOD;
            }
        }
    }

    res
}

fn mul_vec(mat: &IntMat, vec: &IntVec) -> IntVec {
    let n = mat.len();
    let m = vec.len();
    let mut res = vec![0;n];

    for i in 0..n {
        for j in 0..m {
            res[i] = (res[i] + mat[i][j] * vec[j] % MOD) % MOD;
        }
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

impl<A: Read, B: Read, C: Read, D: Read> Read for (A, B, C, D) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]), D::read(tokens[3]))
    }
}
