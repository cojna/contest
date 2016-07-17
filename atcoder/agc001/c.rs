#[allow(unused_imports)]
use std::cmp::{min, max};

type Vertex = usize;
type Edge = (Vertex, Vertex);
type Graph = Vec<Vec<Vertex>>;

fn main(){
    let (n, k): (usize, i64) = readln();

    let mut gr: Graph = vec![vec![];n];
    let mut edges: Vec<Edge> = Vec::with_capacity(n-1);

    for _ in 0..n-1{
        let (a, b): (usize, usize) = readln();
        let a = a - 1;
        let b = b - 1;

        gr[a].push(b);
        gr[b].push(a);
        edges.push((a, b));
    }
    let gr = gr;
    let edges = edges;

    let nothing = std::usize::MAX;

    let mut res = 0;
    if k % 2 == 0{
        for v in 0..n{
            res = max(res, dfs(&gr, k/2, nothing, v));
        }
    } else {
        for (u, v) in edges{
            res = max(res, dfs(&gr, (k-1)/2, u, v) + dfs(&gr, (k-1)/2, v, u));
        }
    }
    println!("{}", n - res);

}

fn dfs(gr: &Graph, depth: i64, prev: usize, v: usize) -> usize{
    if depth < 0 {
        0
    } else {
        let mut res = 1;
        for next in &gr[v]{
            let next = *next;
            if next != prev{
                res += dfs(&gr, depth - 1, v, next);
            }
        }
        res
    }
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
