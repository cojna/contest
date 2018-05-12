#![allow(unused_imports, unused_macros, dead_code)]
use std::f64::*;
use std::cmp::*;
use std::collections::*;

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
    let mut ps: Vec<usize> = io.reads(n);
    for i in 0..n {
        ps[i] = ps[i] - 1;
    }
    let mut uf = UnionFind::new(n);
    for _ in 0..m {
        let (x, y): (usize, usize) = io.read();
        uf.unite(x - 1, y - 1);
    }
    let mut res = 0;
    for i in 0..n {
        if uf.equiv(i, ps[i]) {
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

    pub fn size(&mut self, x: usize) -> usize {
        let px = self.find(x);
        self.size[px]
    }

    pub fn count(&self) -> usize {
        let mut res = 0;
        for &p in &self.parent {
            if p == NOTHING {
                res += 1;
            }
        }
        res
    }
}
fn main() {
    use std::io::*;
    std::thread::Builder::new()
        .stack_size(32 * 1024 * 1024)
        .spawn(|| {
            let stdin = stdin();
            let stdin = BufReader::new(stdin.lock());
            let stdout = stdout();
            let stdout = BufWriter::new(stdout.lock());
            let mut io = IO::new(stdin, stdout);
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
