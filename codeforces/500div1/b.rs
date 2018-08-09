#![allow(unused_imports, unused_macros, dead_code)]
use std::f64::*;
use std::cmp::*;
use std::collections::*;
use std::ops::*;

macro_rules! debug{
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

const A: usize = 'A' as usize;

fn solve<R: std::io::BufRead, W: std::io::Write>(io: &mut IO<R, W>) {
    let (n, m, q): (usize, usize, usize) = io.readln();
    if q == 0 {
        io.writeln(n + m - 1);
        return;
    }
    let mut uf_r = UnionFind::new(n);
    let mut uf_c = UnionFind::new(m);
    let mut rs: Vec<usize> = vec![NOTHING;n];
    let mut cs: Vec<usize> = vec![NOTHING;m];
    let (r0, c0): (usize, usize) = io.readln();
    let r0 = r0 - 1;
    let c0 = c0 - 1;
    rs[r0] = c0;
    cs[c0] = r0;
    for _ in 1..q {
        let (r, c): (usize, usize) = io.readln();
        let r = r - 1;
        let c = c - 1;

        if rs[r] != NOTHING {
            uf_c.unite(rs[r], c);
        } else {
            rs[r] = c;
        }
        if cs[c] != NOTHING {
            uf_r.unite(cs[c], r);
        } else {
            cs[c] = r;
        }
    }

    let mut res = 0;
    for i in 0..n {
        if !uf_r.equiv(i, r0) {
            uf_r.unite(i, r0);
            if rs[i] != NOTHING {
                uf_c.unite(c0, rs[i]);
            }
            res += 1;
        }
    }

    for j in 0..m {
        if !uf_c.equiv(j, c0) {
            uf_c.unite(j, c0);
            if cs[j] != NOTHING {
                uf_r.unite(r0, rs[j]);
            }
            res += 1;
        }
    }

    io.writeln(res);
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
    reader: R,
    writer: W,
}

impl<R: ::std::io::BufRead, W: ::std::io::Write> IO<R, W> {
    pub fn new(r: R, w: W) -> IO<R, W> {
        IO {
            reader: r,
            writer: w,
        }
    }

    pub fn readln<T: UnsafeFromStr>(&mut self) -> T {
        let mut buf = Vec::new();
        self.reader.read_until(b'\n', &mut buf).unwrap();
        let token = unsafe { ::std::str::from_utf8_unchecked(&buf) };
        T::unsafe_from_str(token.trim_right())
    }

    pub fn readlns<T: UnsafeFromStr>(&mut self, n: usize) -> Vec<T> {
        (0..n).map(|_| self.readln()).collect()
    }

    pub fn write<T: ::std::fmt::Display>(&mut self, x: T) {
        self.writer.write_fmt(format_args!("{}", x)).unwrap();
    }
    pub fn writeln<T: ::std::fmt::Display>(&mut self, x: T) {
        self.writer.write_fmt(format_args!("{}\n", x)).unwrap();
    }

    pub fn writes<T: ::std::fmt::Display>(&mut self, xs: &Vec<T>, sep: &str) {
        let mut it = xs.iter();
        for &x in it.next().iter() {
            self.write(x);
        }
        for x in it {
            self.writer.write(sep.as_bytes()).unwrap();
            self.write(x);
        }
    }

    pub fn writesln<T: ::std::fmt::Display>(&mut self, xs: &Vec<T>, sep: &str) {
        self.writes(xs, sep);
        self.writer.write(b"\n").unwrap();
    }

    pub fn flush(&mut self) {
        self.writer.flush().unwrap()
    }
}

pub trait UnsafeFromStr {
    fn unsafe_from_str(s: &str) -> Self;
}

impl UnsafeFromStr for char {
    fn unsafe_from_str(s: &str) -> char {
        s.chars().next().unwrap()
    }
}

impl UnsafeFromStr for Vec<char> {
    fn unsafe_from_str(s: &str) -> Vec<char> {
        s.chars().collect()
    }
}

macro_rules! unsafe_from_str_impl{
    ($($t:ty)*) => ($(
        impl UnsafeFromStr for $t {
            fn unsafe_from_str(s: &str) -> $t{
                s.parse().unwrap()
            }
        }
        impl UnsafeFromStr for Vec<$t> {
            fn unsafe_from_str(s: &str) -> Vec<$t>{
                s.split_whitespace().map(|t| t.parse().unwrap()).collect()
            }
        }
    )*)
}
unsafe_from_str_impl!{ usize u8 u16 u32 u64 isize i8 i16 i32 i64 f32 f64 }

macro_rules! unsafe_from_str_tuple_impl{
    ($($t:ident),+) => {
        impl<$($t: UnsafeFromStr),*> UnsafeFromStr for ($($t),*) {
            fn unsafe_from_str(s: &str) -> Self {
                let mut tokens = s.split_whitespace();
                ($($t::unsafe_from_str(tokens.next().unwrap())),+)
            }
        }
    }
}
unsafe_from_str_tuple_impl!(A, B);
unsafe_from_str_tuple_impl!(A, B, C);
unsafe_from_str_tuple_impl!(A, B, C, D);
unsafe_from_str_tuple_impl!(A, B, C, D, E);
