#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

fn solve() {
    let (n, q): (usize, usize) = readln();

    let mut es: Vec<(i64, i64)> = Vec::with_capacity(n);
    for _ in 0..n {
        let pair: (i64, i64) = readln();
        es.push(pair);
    }

    let mut dist: Vec<Vec<i64>> = Vec::with_capacity(n);
    for i in 0..n {
        let ds: Vec<i64> = readln();
        dist.push(ds);
    }

    let mut uvs: Vec<(usize, usize)> = Vec::with_capacity(q);
    for _ in 0..q {
        let (u, v): (usize, usize) = readln();
        uvs.push((u - 1, v - 1));
    }


    for i in 0..n {
        dist[i][i] = 0;
    }

    for i in 0..n {
        for j in 0..n {
            if dist[i][j] < 0 {
                dist[i][j] = 0x3f3f3f3f3f3f3f3f;
            }
        }
    }

    for k in 0..n {
        for i in 0..n {
            for j in 0..n {
                dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j]);
            }
        }
    }


    let INF: f64 = 0x3f3f3f3f3f3f3f3fi64 as f64;
    let mut d: Vec<Vec<f64>> = vec![vec![INF;n];n];
    for i in 0..n {
        d[i][i] = 0.0;
    }
    for i in 0..n {
        for j in 0..n {
            if dist[i][j] <= es[i].0 && i != j {
                d[i][j] = (dist[i][j] as f64) / (es[i].1 as f64);
            }
        }
    }

    for k in 0..n {
        for i in 0..n {
            for j in 0..n {
                if d[i][j] > d[i][k] + d[k][j] {
                    d[i][j] = d[i][k] + d[k][j];
                }
            }
        }
    }

    for i in 0..q {
        if i < q - 1 {
            print!("{} ", d[uvs[i].0][uvs[i].1]);
        } else {
            println!("{}", d[uvs[i].0][uvs[i].1]);
        }
    }

}

fn main() {
    let t: usize = readln();
    for i in 0..t {
        print!("Case #{}: ", i + 1);
        solve();
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
