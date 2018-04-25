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
    let (n, t): (usize, usize) = io.read();
    let xs: Vec<usize> = io.reads(n);
    let lrs: Vec<(usize, usize)> = io.reads(t);
    let mut qs: Vec<MoQuery> = Vec::with_capacity(t);
    for i in 0..t {
        let (l, r) = lrs[i];
        qs.push(MoQuery::new(l - 1, r - 1, i));
    }
    let mut mo = Mo::new(xs);
    mo.run(&qs);
    io.writes(&mo.result, "\n");
}

struct Mo {
    arr: Vec<usize>,
    freq: Vec<usize>,
    result: Vec<i64>,
}

impl Mo {
    fn new(arr: Vec<usize>) -> Mo {
        Mo {
            arr: arr,
            freq: vec![0;1_000_001],
            result: vec![],
        }
    }

    fn add(&mut self, i: usize) -> i64 {
        let x = self.arr[i];
        let f = self.freq[x];
        self.freq[x] += 1;
        (2 * f + 1) as i64 * x as i64
    }
    fn remove(&mut self, i: usize) -> i64 {
        let x = self.arr[i];
        let f = self.freq[x];
        self.freq[x] -= 1;
        (2 * f - 1) as i64 * x as i64
    }

    fn run(&mut self, queries: &Vec<MoQuery>) {
        let m = queries.len();
        let mut qs = queries.clone();
        self.result = vec![0;m];
        let block_size = 450;
        qs.sort_by(|x, y| {
            let px = (x.l / block_size, x.r);
            let py = (y.l / block_size, y.r);
            px.cmp(&py)
        });
        let mut l = 0;
        let mut r = 0;
        self.freq[self.arr[0]] += 1;
        let mut acc = self.arr[0] as i64;
        for &q in &qs {
            for i in (r + 1)..(q.r + 1) {
                acc += self.add(i);
            }
            for i in ((q.r + 1)..r + 1).rev() {
                acc -= self.remove(i);
            }
            for i in (q.l..l).rev() {
                acc += self.add(i);
            }
            for i in l..q.l {
                acc -= self.remove(i);
            }
            self.result[q.i] = acc;
            l = q.l;
            r = q.r;
        }

    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MoQuery {
    l: usize,
    r: usize,
    i: usize,
}

impl MoQuery {
    fn new(l: usize, r: usize, i: usize) -> MoQuery {
        MoQuery { l: l, r: r, i: i }
    }
}

fn main() {
    use std::io::*;
    let stdin = stdin();
    let stdin = BufReader::new(stdin.lock());
    let stdout = stdout();
    let stdout = BufWriter::new(stdout.lock());
    let mut io = IO::new(stdin, stdout);
    solve(&mut io);
    io.flush();
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
