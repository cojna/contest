#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const INF: i64 = 0x3f3f3f3f;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

fn main() {
    let (h, w, k): (usize, usize, i64) = readln();
    let mut m: Vec<Vec<char>> = Vec::with_capacity(h);
    let mut d: Vec<Vec<i64>> = vec![vec![INF;w];h];
    for _ in 0..h {
        let cs = get_chars();
        m.push(cs);
    }

    let mut start: (usize, usize) = (0, 0);
    for i in 0..h {
        for j in 0..w {
            if m[i][j] == 'S' {
                start = (i, j);
            }
        }
    }

    let mut q = VecDeque::new();
    q.push_back((start, 0));
    d[start.0][start.1] = 0;
    while let Some(((x, y), dist)) = q.pop_front() {
        for i in 0..4 {
            let nx = x.wrapping_add(DX[i]);
            let ny = y.wrapping_add(DY[i]);
            if nx < h && ny < w {
                if m[nx][ny] == '.' && d[nx][ny] == INF {
                    d[nx][ny] = dist + 1;
                    q.push_back(((nx, ny), dist + 1));
                }
            }
        }
    }

    let mut res = INF;
    for i in 0..h {
        for j in 0..w {
            if d[i][j] <= k {
                let &t = vec![i, j, h - i - 1, w - j - 1].iter().min().unwrap();
                res = min(res, (t as i64 + k - 1) / k);
            }
        }
    }

    println!("{}", res + 1);
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
