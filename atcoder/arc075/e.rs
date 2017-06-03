#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const INF: i64 = 0x3f3f3f3f3f3f3f3f;
const NOTHING: usize = std::usize::MAX;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

fn main() {
    let (n, k): (usize, i64) = readln();
    let mut xs: Vec<i64> = Vec::with_capacity(n);
    for _ in 0..n {
        let x: i64 = readln();
        xs.push(x - k);
    }

    let mut cum: Vec<i64> = vec![0;n+1];
    for i in 0..n {
        cum[i + 1] = cum[i] + xs[i];
    }

    let mut ys = cum.clone();
    ys.sort();
    ys.dedup();

    let mut ord: HashMap<i64, usize> = HashMap::new();
    for i in 0..ys.len() {
        ord.insert(ys[i], i);
    }

    let mut bit = BIT::new(ys.len() + 1);
    let mut res = 0;
    for i in 0..n + 1 {
        let &id = ord.get(&cum[i]).unwrap();
        res += bit.query_key(id + 1);
        bit.add(id, 1);
    }

    println!("{}", res);
}

pub struct BIT {
    size: usize,
    data: Vec<i64>,
}

impl BIT {
    fn new(n: usize) -> BIT {
        BIT {
            size: n,
            data: vec![0;n],
        }
    }

    // sum [0, k)
    fn query_key(&self, k: usize) -> i64 {
        let mut res = 0;
        let mut i = k;
        while i > 0 {
            i -= 1;
            res += self.data[i];
            i &= i + 1;
        }
        res
    }

    // sum [l, r)
    fn query_range(&self, l: usize, r: usize) -> i64 {
        debug_assert!(l <= r);
        if l == 0 {
            self.query_key(r)
        } else {
            self.query_key(r) - self.query_key(l)
        }
    }

    fn add(&mut self, k: usize, x: i64) {
        debug_assert!(k < self.size);
        let mut i = k;
        while i < self.size {
            self.data[i] += x;
            i |= i + 1;
        }
    }
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
