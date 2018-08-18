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
    let mut freq: Vec<Vec<isize>> = vec![vec![0;n+1];n+1];
    for _ in 0..m {
        let (l, r): (usize, usize) = io.readln();
        let l = l - 1;
        let r = r - 1;
        freq[0][r] += 1;
        freq[l + 1][r] -= 1;
    }

    for i in 0..n + 1 {
        for j in 1..n + 1 {
            freq[i][j] += freq[i][j - 1];
        }
    }

    for i in 1..n + 1 {
        for j in 0..n + 1 {
            freq[i][j] += freq[i - 1][j];
        }
    }

    for _ in 0..q {
        let (p, q): (usize, usize) = io.readln();
        io.writeln(freq[p - 1][q - 1]);
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
