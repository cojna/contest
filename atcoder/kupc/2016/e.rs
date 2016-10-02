#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn nxny(i: usize, j: usize, h: usize, w: usize) -> Vec<(usize, usize)>{
    let mut nxny: Vec<(usize, usize)> = Vec::with_capacity(4);
    if i != h - 1 { nxny.push((i + 1, j)); }
    if j != w - 1 { nxny.push((i, j + 1)); }
    if i != 0 { nxny.push((i - 1, j)); }
    if j != 0 { nxny.push((i, j - 1)); }
    nxny
}

fn main(){
    let (h, w): (usize, usize) = readln();
    let mut m: Vec<Vec<char>> = vec![vec!();h];
    for i in 0..h{
        m[i] = get_line().chars().collect();
    }

    for i in 0..h{
        for j in 0..w{
            if i == 0 || i == h - 1 || j == 0 || j == w - 1{
                if m[i][j] == 'X'{
                    println!("-1");
                    return;
                }
            }
        }
    }

    let mut gr = MaxFlowGraph::new(2 * h * w + 2);

    fn idx_in(i: usize, j: usize, w: usize) -> usize{
        2 * (i * w + j)
    }

    fn idx_out(i: usize, j: usize, w: usize) -> usize{
        2 * (i * w + j) + 1
    }

    let src_idx = idx_out(h-1, w-1, w) + 1;
    let sink_idx = src_idx + 1;

    for i in 0..h{
        for j in 0..w{
            gr.add_edge(idx_in(i,j,w), idx_out(i,j,w), 1);
            for (ni, nj) in nxny(i,j,h,w){
                gr.add_edge(idx_out(i,j,w), idx_in(ni,nj,w), INF);
            }
            if i == 0 || i == h - 1 || j == 0 || j == w - 1 {
                gr.add_edge(idx_out(i,j,w), sink_idx, INF);
            }
            if m[i][j] == 'X' {
                gr.add_edge(src_idx, idx_out(i,j,w), INF);
            }
        }
    }

    let res = gr.max_flow(src_idx, sink_idx);
    println!("{}", res);

}

type Capacity = i64;
const INF: Capacity = 0x3f3f3f3f3f3f3f3f;

#[derive(Debug, Clone, Copy)]
struct Edge {
    dst: usize,
    cap: Capacity,
    rev: usize
}

struct MaxFlowGraph {
    num_v: usize,
    adj: Vec<Vec<Edge>>,
    level: Vec<isize>,
    iter: Vec<usize>
}

impl MaxFlowGraph {
    fn new(size: usize) -> MaxFlowGraph {
        MaxFlowGraph {
            num_v: size,
            adj: vec![vec!(); size],
            level: vec![-1; size],
            iter: vec![0; size]
        }
    }

    fn add_edge(&mut self, src: usize, dst: usize, cap: Capacity){
        let rev = self.adj[dst].len();
        self.adj[src].push(Edge{dst: dst, cap: cap, rev: rev});
        let rev = self.adj[src].len() - 1;
        self.adj[dst].push(Edge{dst: src, cap: 0, rev: rev});
    }

    fn bfs(&mut self, src: usize){
        self.level = vec![-1; self.num_v];
        let mut que = VecDeque::new();
        self.level[src] = 0;
        que.push_back(src);
        loop {
            match que.pop_front() {
                Some(v) => {
                    for e in &self.adj[v]{
                        if e.cap > 0 && self.level[e.dst] < 0{
                            self.level[e.dst] = self.level[v] + 1;
                            que.push_back(e.dst);
                        }
                    }
                },
                None => {
                    break;
                }
            }
        }
    }

    fn dfs(&mut self, v: usize, sink: usize, flow: Capacity) -> Capacity{
        if v == sink { return flow; }
        while self.iter[v] < self.adj[v].len(){
            let i = self.iter[v];
            let e = self.adj[v][i];
            self.iter[v] += 1;
            if e.cap > 0 && self.level[v] < self.level[e.dst]{
                let d = self.dfs(e.dst, sink, min(flow, e.cap));
                if d > 0 {
                    self.adj[v][i].cap -= d;
                    self.adj[e.dst][e.rev].cap += d;
                    return d;
                }
            }
        }
        return 0;
    }

    fn max_flow(&mut self, src: usize, sink: usize) -> Capacity{
        let mut flow = 0;
        loop {
            self.bfs(src);
            if self.level[sink] < 0 {
                return flow;
            }

            self.iter = vec![0; self.num_v];

            loop {
                let f = self.dfs(src, sink, INF);
                if f <= 0 { break; }
                flow += f;
            }
        }
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