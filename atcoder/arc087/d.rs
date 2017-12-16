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
    let cs: Vec<char> = readln();
    let (gx, gy): (isize, isize) = readln();

    let mut start = 0;
    for &c in &cs {
        if c == 'F' {
            start += 1;
        } else {
            break;
        }
    }
    if start == cs.len() {
        if (gx, gy) == (cs.len() as isize, 0) {
            println!("Yes");
        } else {
            println!("No");
        }
        return;
    }

    let gx = gx - start as isize;

    let mut xs = vec![];
    let mut ys = vec![];

    let mut f_len = 0;
    let mut dir = 0;
    for i in start..cs.len() {
        let c = cs[i];
        if c == 'F' {
            f_len += 1;
        } else {
            if f_len > 0 {
                if dir == 0 {
                    xs.push(f_len);
                } else {
                    ys.push(f_len);
                }
            }
            dir = 1 - dir;
            f_len = 0;
        }
    }

    if f_len > 0 {
        if dir == 0 {
            xs.push(f_len);
        } else {
            ys.push(f_len);
        }
    }

    let mut x_set: HashSet<isize> = HashSet::new();
    x_set.insert(0);
    for &x in &xs {
        let mut s: HashSet<isize> = HashSet::new();
        for &ex in &x_set {
            s.insert(ex + x);
            s.insert(ex - x);
        }
        x_set = s;
    }

    let mut y_set: HashSet<isize> = HashSet::new();
    y_set.insert(0);
    for &y in &ys {
        let mut s: HashSet<isize> = HashSet::new();
        for &ey in &y_set {
            s.insert(ey + y);
            s.insert(ey - y);
        }
        y_set = s;
    }

    if x_set.contains(&gx) && y_set.contains(&gy) {
        println!("Yes");
        return;
    }
    println!("No");
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
