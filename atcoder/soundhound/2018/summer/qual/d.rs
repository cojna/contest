#![allow(unused_imports, unused_macros, dead_code)]
use std::f64::*;
use std::cmp::*;
use std::collections::*;
use std::ops::*;

macro_rules! dump{
    ($($a:expr),*) => {
        #[cfg(debug_assertions)]
        eprintln!(
            concat!("{}:{}:{}: ",$(stringify!($a), " = {:?}, "),*),
            file!(), line!(), column!(), $($a),*
        );
        #[cfg(not(debug_assertions))]
        {};
    }
}

const INF: i64 = 0x3f3f3f3f3f3f3f3f;
const MOD: i64 = 1000000007;
const NOTHING: usize = std::usize::MAX;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

fn solve<R: std::io::Read, W: std::io::Write>(io: &mut IO<R, W>) {
    let (n, m, s, t): (usize, usize, usize, usize) = io.read();
    let s = s - 1;
    let t = t - 1;
    let mut grs: Graph<(), (usize, usize, i64)> = Graph::new(n);
    let mut grt: Graph<(), (usize, usize, i64)> = Graph::new(n);
    for _ in 0..m {
        let (u, v, a, b): (usize, usize, i64, i64) = io.read();
        let u = u - 1;
        let v = v - 1;
        grs.add_edge((u, v, a));
        grs.add_edge((v, u, a));
        grt.add_edge((u, v, b));
        grt.add_edge((v, u, b));
    }
    let from_s = grs.spfa(s).unwrap();
    let from_t = grt.spfa(t).unwrap();
    dump!(from_s);
    dump!(from_t);
    let mut cost = vec![];
    for i in 0..n {
        cost.push(from_s[i] + from_t[i]);
    }
    let rmq = RMQ::from_vec(&cost);
    for i in 0..n {
        // io.writeln(rmq.min(i..n));
        io.writeln(1_00000_00000_00000i64 - rmq.min(i..n));
    }
}
pub type VertexId = usize;

pub trait Edge {
    fn src(&self) -> VertexId;
    fn dst(&self) -> VertexId;
    fn reverse(&self) -> Self;
}
pub trait HasWeight {
    fn weight(&self) -> i64;
}

impl Edge for (usize, usize, i64) {
    fn src(&self) -> usize {
        self.0
    }
    fn dst(&self) -> usize {
        self.1
    }
    fn reverse(&self) -> Self {
        (self.1, self.0, self.2)
    }
}

impl HasWeight for (usize, usize, i64) {
    fn weight(&self) -> i64 {
        self.2
    }
}

pub struct Graph<V, E> {
    pub num_v: usize,
    pub vertices: Vec<V>,
    pub adj: Vec<Vec<E>>,
}

impl<E: Edge + Clone> Graph<(), E> {
    pub fn new(num_v: usize) -> Graph<(), E> {
        Graph {
            num_v: num_v,
            vertices: vec![],
            adj: vec![vec![];num_v],
        }
    }
}

impl<V, E: Edge + Clone> Graph<V, E> {
    pub fn add_edge(&mut self, e: E) {
        self.adj[e.src()].push(e.clone());
    }

    pub fn edges(&self) -> Vec<E> {
        self.adj
            .clone()
            .into_iter()
            .flat_map(|x| x.into_iter())
            .collect()
    }
}
pub trait SPFA<E> {
    fn spfa(&self, start: VertexId) -> Option<Vec<i64>>;
}

impl<V, E: Edge + HasWeight + Clone> SPFA<E> for Graph<V, E> {
    fn spfa(&self, start: VertexId) -> Option<Vec<i64>> {
        let mut q = VecDeque::new();
        let mut in_q = vec![false;self.num_v];
        let mut freq = vec![0usize;self.num_v];
        let mut dist = vec![INF; self.num_v];
        dist[start] = 0;
        q.push_back(start);
        in_q[start] = true;
        while let Some(v) = q.pop_front() {
            freq[v] += 1;
            if freq[v] >= self.num_v {
                return None;
            }
            for e in &self.adj[v] {
                if dist[v] + e.weight() < dist[e.dst()] {
                    dist[e.dst()] = dist[v] + e.weight();
                    if !in_q[e.dst()] {
                        if let Some(&f) = q.front() {
                            if dist[e.dst()] < dist[f] {
                                q.push_front(e.dst());
                            } else {
                                q.push_back(e.dst());
                            }
                        } else {
                            q.push_back(e.dst());
                        }
                        in_q[e.dst()] = true;
                    }
                }
                in_q[v] = false;
            }
        }
        Some(dist)
    }
}

pub struct RMQ<T> {
    table: Vec<Vec<(T, usize)>>,
}

impl<T: Ord + Copy> RMQ<T> {
    pub fn from_vec(vec: &Vec<T>) -> RMQ<T> {
        let n: usize = vec.len();
        let log_n: usize = floor_log2(n);
        let mut vec0: Vec<(T, usize)> = Vec::with_capacity(n);
        for (i, &x) in vec.iter().enumerate() {
            vec0.push((x, i));
        }
        let mut table: Vec<Vec<(T, usize)>> = Vec::with_capacity(log_n + 1);
        table.push(vec0);
        for i in 1..log_n + 1 {
            let step = 1 << (i - 1);
            let mut vec_i = table[i - 1].clone();
            for j in 0..n - step {
                vec_i[j] = min(table[i - 1][j], table[i - 1][j + step]);
            }
            table.push(vec_i);
        }
        RMQ { table: table }
    }

    pub fn min(&self, range: Range<usize>) -> T {
        self.query(range).0
    }

    pub fn min_index(&self, range: Range<usize>) -> usize {
        self.query(range).1
    }

    fn query(&self, range: Range<usize>) -> (T, usize) {
        debug_assert!(range.start < range.end);
        let l = range.start;
        let r = range.end;
        if r - l > 1 {
            let log_d = floor_log2(r - l - 1);
            min(self.table[log_d][l], self.table[log_d][r - (1 << log_d)])
        } else {
            self.table[0][l]
        }
    }
}

pub fn floor_log2(x: usize) -> usize {
    (unsafe { std::mem::transmute::<f64, u64>(x as f64) } >> 52) as usize - 1023
}

fn main() {
    use std::io::*;
    std::thread::Builder::new()
        .stack_size(32 * 1024 * 1024)
        .spawn(|| {
            let stdin = stdin();
            let stdout = stdout();
            let mut io = IO::new(BufReader::new(stdin.lock()), BufWriter::new(stdout.lock()));
            solve(&mut io);
            io.flush();
        })
        .unwrap()
        .join()
        .unwrap();
}

pub struct IO<R, W> {
    reader: std::io::Bytes<R>,
    writer: W,
}

impl<R: std::io::Read, W: std::io::Write> IO<R, W> {
    pub fn new(r: R, w: W) -> IO<R, W> {
        IO {
            reader: r.bytes(),
            writer: w,
        }
    }

    pub fn read<T: Read>(&mut self) -> T {
        T::read(&mut self.reader)
    }

    pub fn reads<T: Read>(&mut self, n: usize) -> Vec<T> {
        (0..n).map(|_| T::read(&mut self.reader)).collect()
    }

    pub fn write<T: std::fmt::Display>(&mut self, x: T) {
        self.writer.write_fmt(format_args!("{}", x)).unwrap();
    }
    pub fn writeln<T: std::fmt::Display>(&mut self, x: T) {
        self.writer.write_fmt(format_args!("{}\n", x)).unwrap();
    }

    pub fn writes<T: std::fmt::Display>(&mut self, xs: &Vec<T>, sep: &str) {
        let mut it = xs.iter();
        for &x in it.next().iter() {
            self.write(x);
        }
        for x in it {
            self.writer.write(sep.as_bytes()).unwrap();
            self.write(x);
        }
    }

    pub fn writesln<T: std::fmt::Display>(&mut self, xs: &Vec<T>, sep: &str) {
        self.writes(xs, sep);
        self.writer.write(b"\n").unwrap();
    }

    pub fn flush(&mut self) {
        self.writer.flush().unwrap()
    }
}

pub trait Read {
    fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self;
}

#[inline(always)]
pub fn is_whitespace(x: u8) -> bool {
    x < 33u8 || 126u8 < x
}

macro_rules! read_int_impl{
    ($($t:ty)*) => ($(
        impl Read for $t {
            fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self {
                let head: u8 = bytes
                    .map(|x| x.unwrap())
                    .skip_while(|&x| is_whitespace(x))
                    .next()
                    .unwrap();
                if head == 45u8 {
                    bytes.map(|x| x.unwrap())
                        .take_while(|&x| !is_whitespace(x))
                        .fold(0, |a, x| 10 * a - x as $t + 48)
                } else {
                    bytes.map(|x| x.unwrap())
                        .take_while(|&x| !is_whitespace(x))
                        .fold((head - 48) as $t, |a, x| 10 * a + x as $t - 48)
                }
            }
        }
    )*)
}
read_int_impl! { isize i8 i16 i32 i64 usize u8 u16 u32 u64}

macro_rules! read_float_impl{
    ($($t:ty)*) => ($(
        impl Read for $t {
            fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self {
                String::read(bytes).as_str().parse::<$t>().unwrap()
            }
        }
    )*)
}
read_float_impl! { f32 f64 }

impl Read for char {
    fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self {
        bytes.map(|x| x.unwrap())
            .skip_while(|&x| is_whitespace(x))
            .next()
            .unwrap() as char
    }
}

impl Read for String {
    fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self {
        bytes.map(|x| x.unwrap())
            .skip_while(|&x| is_whitespace(x))
            .take_while(|&x| !is_whitespace(x))
            .map(|x| x as char)
            .collect()
    }
}

impl Read for Vec<char> {
    fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self {
        bytes.map(|x| x.unwrap())
            .skip_while(|&x| is_whitespace(x))
            .take_while(|&x| !is_whitespace(x))
            .map(|x| x as char)
            .collect()
    }
}

macro_rules! read_tuple_impl{
    ($($t:ident),+) => {
        impl<$($t: Read),*> Read for ($($t),*) {
            fn read<R: std::io::Read>(bytes: &mut std::io::Bytes<R>) -> Self {
                ($($t::read(bytes)),+)
            }
        }
    }
}
read_tuple_impl!(A, B);
read_tuple_impl!(A, B, C);
read_tuple_impl!(A, B, C, D);
read_tuple_impl!(A, B, C, D, E);
