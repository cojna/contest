#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const INF: i64 = 0x3f3f3f3f;

fn main() {
    let n: usize = readln();
    let mut c: Vec<Vec<char>> = Vec::with_capacity(n);
    for _ in 0..n {
        let cs = get_chars();
        c.push(cs);
    }

    let mut has_black = false;
    for i in 0..n {
        for j in 0..n {
            if c[i][j] == '#' {
                has_black = true;
                break;
            }
        }
    }

    if !has_black {
        println!("-1");
        return;
    }

    let mut res = INF;

    for i in 0..n {
        let mut acc = 0;
        let mut has_black = false;
        for j in 0..n {
            if c[j][i] == '#' {
                has_black = true;
                break;
            }
        }
        if !has_black {
            acc += 1
        }
        for j in 0..n {
            if c[i][j] == '.' {
                acc += 1;
            }
        }
        res = min(acc, res);

    }

    for i in 0..n {
        let mut has_white = false;
        for j in 0..n {
            if c[j][i] == '.' {
                has_white = true;
                break;
            }
        }
        if has_white {
            res += 1;
        }
    }

    println!("{}", res);
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
