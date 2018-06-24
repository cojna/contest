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
    let (n, m): (usize, usize) = io.read();
    let mut g: Vec<Vec<bool>> = vec![vec![true;n];n];
    for i in 0..n {
        g[i][i] = false;
    }

    for _ in 0..m {
        let (x, y): (usize, usize) = io.read();
        let x = x - 1;
        let y = y - 1;
        g[x][y] = false;
        g[y][x] = false;
    }

    let mut bg = BipartiteGraph::new(n);
    for i in 0..n {
        for j in i + 1..n {
            if g[i][j] {
                bg.add_edge(i, j);
            }
        }
    }

    if !bg.is_bipartite() {
        io.writeln("-1");
        return;
    }
    let components = bg.components().unwrap();
    dump!(components);

    let mut set: Vec<bool> = vec![false;n+1];
    set[0] = true;
    for component in components {
        let s0 = component.color0.len();
        let s1 = component.color1.len();
        let mut new = vec![false;n+1];
        for i in (0..n + 1).rev() {
            if s0 <= i {
                new[i] |= set[i - s0];
            }
            if s1 <= i {
                new[i] |= set[i - s1];
            }
        }
        std::mem::swap(&mut set, &mut new);
    }


    let mut res = m;
    for s in 1..n {
        if set[s] {
            res = min(res, s * (s - 1) / 2 + (n - s) * (n - s - 1) / 2);
        }
    }

    io.writeln(res)

}

#[derive(Debug)]
pub struct BipartiteComponent {
    color0: Vec<usize>,
    color1: Vec<usize>,
}

pub struct BipartiteGraph {
    num_v: usize,
    uf: UnionFind,
    is_bipartite_cache: bool,
}

impl BipartiteGraph {
    pub fn new(num_v: usize) -> BipartiteGraph {
        BipartiteGraph {
            num_v: num_v,
            uf: UnionFind::new(2 * num_v),
            is_bipartite_cache: true,
        }
    }

    pub fn add_edge(&mut self, x: usize, y: usize) {
        debug_assert!(x < self.num_v && y < self.num_v);
        self.uf.unite(2 * x, 2 * y + 1);
        self.uf.unite(2 * y, 2 * x + 1);
    }

    pub fn is_bipartite(&mut self) -> bool {
        if !self.is_bipartite_cache {
            return false;
        }

        for i in 0..self.num_v {
            if self.uf.equiv(2 * i, 2 * i + 1) {
                self.is_bipartite_cache = false;
                return false;
            }
        }
        true
    }

    pub fn components(&mut self) -> Option<Vec<BipartiteComponent>> {
        if !self.is_bipartite_cache {
            return None;
        }
        let mut reprs = HashSet::new();
        for i in 0..self.num_v {
            let p0 = self.uf.find(2 * i);
            let p1 = self.uf.find(2 * i + 1);
            if p0 == p1 {
                self.is_bipartite_cache = false;
                return None;
            } else if !reprs.contains(&p0) && !reprs.contains(&p1) {
                reprs.insert(p0);
            }
        }

        Some(self.uf
            .groups()
            .iter()
            .filter(|&(repr, _)| reprs.contains(repr))
            .map(|(_, group)| {
                let (color0, color1): (Vec<usize>, Vec<usize>) = group.into_iter()
                    .partition(|&x| x & 1 == 0);
                BipartiteComponent {
                    color0: color0.iter().map(|&x| x >> 1).collect(),
                    color1: color1.iter().map(|&x| x >> 1).collect(),
                }
            })
            .collect())
    }
}

pub struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
    size: Vec<usize>,
}

impl UnionFind {
    pub fn new(n: usize) -> UnionFind {
        UnionFind {
            parent: vec![NOTHING; n],
            rank: vec![0; n],
            size: vec![1; n],
        }
    }

    pub fn iter(&self) -> Range<usize> {
        0..self.parent.len()
    }

    pub fn find(&mut self, x: usize) -> usize {
        if self.parent[x] == NOTHING {
            x
        } else {
            let px = self.parent[x];
            let root = self.find(px);
            self.parent[x] = root;
            root
        }
    }

    pub fn unite(&mut self, x: usize, y: usize) {
        let px = self.find(x);
        let py = self.find(y);

        if px == py {
            return;
        }

        if self.rank[px] < self.rank[py] {
            self.parent[px] = py;
            self.size[py] += self.size[px];
        } else {
            self.parent[py] = px;
            self.size[px] += self.size[py];
            if self.rank[px] == self.rank[py] {
                self.rank[px] += 1;
            }
        }

    }

    pub fn equiv(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
    }

    pub fn group_size(&mut self, x: usize) -> usize {
        let px = self.find(x);
        self.size[px]
    }

    pub fn groups(&mut self) -> HashMap<usize, Vec<usize>> {
        let mut res = HashMap::new();
        for i in self.iter() {
            let p = self.find(i);
            let ref mut gp = res.entry(p).or_insert(vec![]);
            gp.push(i);
        }
        res
    }

    pub fn count_group(&self) -> usize {
        self.parent.iter().filter(|&p| *p == NOTHING).collect::<Vec<_>>().len()
    }
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
