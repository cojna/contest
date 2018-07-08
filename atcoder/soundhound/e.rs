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
const NOTHING: i64 = std::i64::MAX;
const DX: [usize; 4] = [1, 0, std::usize::MAX, 0];
const DY: [usize; 4] = [0, 1, 0, std::usize::MAX];

fn solve<R: std::io::Read, W: std::io::Write>(io: &mut IO<R, W>) {
    let (n, m): (usize, usize) = io.read();
    let mut gr = vec![vec![];n];
    for i in 0..m {
        let (u, v, s): (usize, usize, i64) = io.read();
        let u = u - 1;
        let v = v - 1;
        gr[u].push((v, s, 2 * i));
        gr[v].push((u, s, 2 * i + 1));
    }

    let mut stack = vec![];
    let mut used: Vec<bool> = vec![false;2*m];
    let mut pos: Vec<i64> = vec![NOTHING;n]; // pos + root
    let mut neg: Vec<i64> = vec![NOTHING;n]; // neg - root
    let root = 0;
    pos[root] = 0;
    stack.push(root);
    while let Some(v) = stack.pop() {
        for &(u, s, i) in &gr[v] {
            if used[i] {
                continue;
            }
            used[i] = true;
            used[i ^ 1] = true;
            let mut updated = false;

            if pos[v] != NOTHING {
                if neg[u] != NOTHING && neg[u] != s - pos[v] {
                    io.writeln(0);
                    return;
                }
                neg[u] = s - pos[v];
                updated = true;
            }

            if neg[v] != NOTHING {
                if pos[u] != NOTHING && pos[u] != s - neg[v] {
                    io.writeln(0);
                    return;
                }
                pos[u] = s - neg[v];
                updated = true;
            }

            if updated {
                stack.push(u);
            }
        }
    }

    let mut min_pos = std::i64::MAX;
    let mut min_neg = std::i64::MAX;
    let mut x = NOTHING;
    for i in 0..n {
        if pos[i] != NOTHING && neg[i] != NOTHING {
            let d = neg[i] - pos[i];
            if d % 2 == 0 && d >= 2 {
                if x == NOTHING || x == d / 2 {
                    x = d / 2;
                } else {
                    io.writeln(0);
                    return;
                }
            }
        }
        if neg[i] != NOTHING {
            min_neg = min(min_neg, neg[i]);
        }
        if pos[i] != NOTHING {
            min_pos = min(min_pos, pos[i]);
        }
    }

    for i in 0..2 * m {
        assert!(used[i]);
    }
    assert_ne!(min_pos, std::i64::MAX);
    assert_ne!(min_neg, std::i64::MAX);

    if x != NOTHING {
        if min_pos + x > 0 && min_neg - x > 0 {
            io.writeln(1);
        } else {
            io.writeln(0);
        }
    } else {
        io.writeln(max(0, min_neg - max(1, 1 - min_pos)));
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
