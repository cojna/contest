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
    let mut gr: Vec<Vec<usize>> = vec![vec![];n];
    for _ in 0..m {
        let (x, y): (usize, usize) = io.read();
        gr[x - 1].push(y - 1);
        gr[y - 1].push(x - 1);
    }
    let mut scc = SCC::new(&gr);
    let components = scc.run();
    let l = components.len();
    let mut res = 0;
    for i in 0..l {
        let mut flag = true;
        for &v in &components[i] {
            if gr[v].len() != 2 {
                flag = false;
                break;
            }
        }
        if flag {
            res += 1;
        }
    }
    io.writeln(res);
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

type VertexID = usize;
type Graph = Vec<Vec<VertexID>>;

pub struct SCC {
    num_v: usize,
    adj: Vec<Vec<VertexID>>,
    lowlink: Vec<usize>,
    preord: Vec<usize>,
    stack: Vec<VertexID>,
    on_stack: Vec<bool>,
    index: usize,
    components: Vec<Vec<VertexID>>,
}

impl SCC {
    pub fn new(gr: &Vec<Vec<VertexID>>) -> SCC {
        SCC {
            num_v: gr.len(),
            adj: gr.clone(),
            lowlink: vec![NOTHING;gr.len()],
            preord: vec![NOTHING;gr.len()],
            stack: vec![],
            on_stack: vec![false;gr.len()],
            index: 0,
            components: vec![],
        }
    }

    fn dfs(&mut self, v: VertexID) {
        self.preord[v] = self.index;
        self.lowlink[v] = self.index;
        self.index += 1;
        self.stack.push(v);
        self.on_stack[v] = true;

        for i in 0..self.adj[v].len() {
            let nv = self.adj[v][i];
            if self.preord[nv] == NOTHING {
                self.dfs(nv);
                self.lowlink[v] = min(self.lowlink[v], self.lowlink[nv]);
            } else if self.on_stack[nv] {
                self.lowlink[v] = min(self.lowlink[v], self.preord[nv]);
            }
        }

        if self.lowlink[v] == self.preord[v] {
            let mut component = vec![];
            while let Some(w) = self.stack.pop() {
                component.push(w);
                self.on_stack[w] = false;
                if v == w {
                    break;
                }
            }
            self.components.push(component);
        }
    }

    pub fn run(&mut self) -> Vec<Vec<VertexID>> {
        for v in 0..self.num_v {
            if self.preord[v] == NOTHING {
                self.dfs(v);
            }
        }
        self.components.clone()
    }
}
