#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

const INF: i32 = 0x3f3f3f3f;

fn main(){
    let (n, m, t): (usize, usize, i32) = readln();
    let mut gr: Vec<Vec<(i32, usize)>> = vec![vec!(); n];

    for _ in 0..m{
        let (ui, vi, ti): (usize, usize, i32) = readln();
        gr[ui-1].push((ti, vi-1));
    }

    let mut dp: Vec<Vec<i32>> = vec![vec![INF; n]; n+1];
    let mut prev: Vec<Vec<usize>> = vec![vec![0; n]; n+1];
    for i in 0..n{
        for j in 0..n{
            prev[i][j] = j;
        }
    }

    dp[0][0] = 0;

    for i in 0..n{
        for v in 0..n{
            if dp[i][v] == INF{
                continue;
            }
            for &(w, u) in &gr[v]{
                if dp[i][v] + w <= t && dp[i][v] + w < dp[i+1][u]{
                    dp[i+1][u] = dp[i][v] + w;
                    prev[i+1][u] = v;
                }
            }
        }
    }

    let mut l = 0;
    let goal = n - 1;
    for i in (0..n+1).rev(){
        if dp[i][goal] <= t{
            l = i;
            break;
        }
    }

    let start = 0;
    let mut v = goal;
    let mut res = Vec::with_capacity(l + 1);
    res.push(v + 1);

    while v != start{
        v = prev[l][v];
        l = l - 1;
        res.push(v + 1);
    }

    res.reverse();

    let res_str: Vec<String> = res.iter().map(|x|x.to_string()).collect();
    println!("{}", res.len());

    println!("{}", res_str.join(" "));
}



#[allow(dead_code)]
fn get_line() -> String{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_string()
}

#[allow(dead_code)]
fn readln<T: Read>() -> T{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    T::read(buf.trim_right())
}

trait Read{
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

impl<T: Read> Read for Vec<T>{
    fn read(s: &str) -> Self {
        s.split_whitespace().map(T::read).collect()
    }
}

impl<A: Read, B: Read> Read for (A, B){
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]))
    }
}

impl<A: Read, B: Read, C: Read> Read for (A, B, C){
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]))
    }
}