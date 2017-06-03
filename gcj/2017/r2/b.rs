#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const INF: i64 = 0x3f3f3f3f;
const NOTHING: usize = std::usize::MAX;

fn solve_small() {
    let (n, _, m): (usize, usize, usize) = readln();
    let mut freq1: Vec<usize> = vec![0;n];
    let mut freq2: Vec<usize> = vec![0;n];

    for _ in 0..m {
        let (p, b): (usize, usize) = readln();
        let p = p - 1;
        if b == 1 {
            freq1[p] += 1;
        } else {
            freq2[p] += 1;
        }
    }

    let src: usize = 4 * n;
    let sink: usize = src + 1;
    let num_v = 4 * n + 2;
    let mut mf = MaxFlow::new(num_v);
    let mut mcf = MinCostFlow::new(num_v);

    for i in 0..n {
        mf.add_edge(src, 4 * i, INF);
        mcf.add_edge(src, 4 * i, INF, 0);

        if freq1[i] > 0 {
            mf.add_edge(4 * i, 4 * i + 1, freq1[i] as i64);
            mcf.add_edge(4 * i, 4 * i + 1, freq1[i] as i64, 0);
        }
        if freq2[i] > 0 {
            mf.add_edge(4 * i + 2, 4 * i + 3, freq2[i] as i64);
            mcf.add_edge(4 * i + 2, 4 * i + 3, freq2[i] as i64, 0);
        }

        mf.add_edge(4 * i + 3, sink, INF);
        mcf.add_edge(4 * i + 3, sink, INF, 0);
    }

    for i in 0..n {
        for j in 0..n {
            if !(i == 0 && j == 0) && freq1[i] > 0 && freq2[j] > 0 {
                mf.add_edge(4 * i + 1, 4 * j + 2, INF);
                let cost: i64 = if i == j { 1 } else { 0 };
                mcf.add_edge(4 * i + 1, 4 * j + 2, INF, cost);
            }
        }
    }

    let matching: Capacity = mf.run(src, sink);
    println!("{} {}",
             m as i64 - matching,
             mcf.run(src, sink, matching).unwrap());
}

fn main() {
    let t: usize = readln();
    for i in 0..t {
        print!("Case #{}: ", i + 1);
        solve_small();
    }
}

type VertexID = usize;
type EdgeID = usize;

type Capacity = i64;
type Cost = i64;

pub struct MaxFlow {
    num_v: usize,
    num_e: usize,
    adj: Vec<Vec<EdgeID>>,
    src: Vec<VertexID>,
    dst: Vec<VertexID>,
    residual: Vec<Capacity>,
    level: Vec<usize>,
    iter: Vec<usize>,
}

impl MaxFlow {
    fn new(num_v: usize) -> MaxFlow {
        MaxFlow {
            num_v: num_v,
            num_e: 0,
            adj: vec![vec!(); num_v],
            src: vec![],
            dst: vec![],
            residual: vec![],
            level: vec![],
            iter: vec![0; num_v],
        }
    }

    fn rev_edge(e: EdgeID) -> EdgeID {
        e ^ 1
    }

    fn add_edge(&mut self, src: usize, dst: usize, cap: Capacity) {
        let i = self.num_e;

        self.adj[src].push(i);
        self.src.push(src);
        self.residual.push(cap);
        self.dst.push(dst);

        self.adj[dst].push(i + 1);
        self.src.push(dst);
        self.residual.push(0);
        self.dst.push(src);

        self.num_e += 2;
    }

    fn bfs(&mut self, src: VertexID) {
        self.level = vec![NOTHING; self.num_v];
        let mut que = VecDeque::new();
        self.level[src] = 0;
        que.push_back(src);
        while let Some(v) = que.pop_front() {
            for &e in &self.adj[v] {
                let nv = self.dst[e];
                if self.residual[e] > 0 && self.level[nv] == NOTHING {
                    self.level[nv] = self.level[v] + 1;
                    que.push_back(nv);
                }
            }
        }
    }

    fn dfs(&mut self, v: VertexID, sink: VertexID, flow: Capacity) -> Capacity {
        if v == sink {
            return flow;
        }
        while self.iter[v] < self.adj[v].len() {
            let i = self.iter[v];
            let e = self.adj[v][i];
            let nv = self.dst[e];
            self.iter[v] += 1;
            if self.residual[e] > 0 && self.level[v] < self.level[nv] {
                let cap = self.residual[e];
                let d = self.dfs(nv, sink, min(flow, cap));
                if d > 0 {
                    self.residual[e] -= d;
                    self.residual[Self::rev_edge(e)] += d;
                    return d;
                }
            }
        }
        return 0;
    }

    fn run(&mut self, src: VertexID, sink: VertexID) -> Capacity {
        let mut flow = 0;
        loop {
            self.bfs(src);
            if self.level[sink] == NOTHING {
                return flow;
            }

            self.iter = vec![0; self.num_v];

            loop {
                let f = self.dfs(src, sink, INF);
                if f <= 0 {
                    break;
                }
                flow += f;
            }
        }
    }
}

pub struct MinCostFlow {
    num_v: usize,
    num_e: usize,
    adj: Vec<Vec<EdgeID>>,
    src: Vec<VertexID>,
    dst: Vec<VertexID>,
    cost: Vec<Cost>,
    residual: Vec<Capacity>,
}

impl MinCostFlow {
    pub fn new(num_v: usize) -> Self {
        MinCostFlow {
            num_v: num_v,
            num_e: 0,
            adj: vec![vec![]; num_v],
            src: vec![],
            dst: vec![],
            cost: vec![],
            residual: vec![],
        }
    }

    pub fn add_edge(&mut self, src: usize, dst: usize, cap: Capacity, cost: Cost) {
        let i = self.num_e;

        self.adj[src].push(i);
        self.src.push(src);
        self.residual.push(cap);
        self.cost.push(cost);
        self.dst.push(dst);

        self.adj[dst].push(i + 1);
        self.src.push(dst);
        self.residual.push(0);
        self.cost.push(-cost);
        self.dst.push(src);

        self.num_e += 2;
    }

    fn rev_edge(e: EdgeID) -> EdgeID {
        e ^ 1
    }

    pub fn run(&mut self, src: VertexID, sink: VertexID, flow: Capacity) -> Option<Cost> {
        let mut res = 0;
        let mut flow = flow;

        let mut potential = vec![0;self.num_v];
        let mut prev_v = vec![NOTHING;self.num_v];
        let mut prev_e = vec![NOTHING;self.num_v];

        while flow > 0 {
            let mut heap: MinHeap<(Cost, VertexID)> = MinHeap::new();
            let mut dist = vec![INF;self.num_v];
            dist[src] = 0;
            heap.push(Down((0, src)));

            // dijkstra
            while let Some(Down((d, v))) = heap.pop() {
                if dist[v] < d {
                    continue;
                }

                for &e in &self.adj[v] {
                    let nv = self.dst[e];
                    let dnv = dist[v] + self.cost[e] + potential[v] - potential[nv];
                    if self.residual[e] > 0 && dist[nv] > dnv {
                        dist[nv] = dnv;
                        prev_v[nv] = v;
                        prev_e[nv] = e;
                        heap.push(Down((dnv, nv)));
                    }

                }
            }

            if dist[sink] == INF {
                return None;
            }

            for v in 0..self.num_v {
                potential[v] += dist[v];
            }

            let mut d = flow;
            let mut v = sink;
            while v != src {
                d = min(d, self.residual[prev_e[v]]);
                v = prev_v[v];
            }
            flow -= d;
            res += d * potential[sink];

            v = sink;
            while v != src {
                self.residual[prev_e[v]] -= d;
                self.residual[Self::rev_edge(prev_e[v])] += d;
                v = prev_v[v];
            }

        }
        Some(res)
    }
}

#[derive(PartialEq, Eq)]
struct Down<T>(T);

impl<T> PartialOrd for Down<T>
    where T: PartialOrd
{
    fn partial_cmp(&self, other: &Down<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T> Ord for Down<T>
    where T: Ord
{
    fn cmp(&self, other: &Down<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}

type MinHeap<T> = BinaryHeap<Down<T>>;


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
