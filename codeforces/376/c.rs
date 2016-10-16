#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let (n, m, k): (usize, usize, usize) = readln();
    let color: Vec<usize> = readln();
    let mut uf = UnionFind::new(n);
    for _ in 0..m{
        let (l, r): (usize, usize) = readln();
        uf.unite(l-1, r-1)
    }
    let mut res = 0;
    let mut repr = Vec::with_capacity(n);
    for i in 0..n{
        repr.push((uf.find(i), color[i]));
    }
    repr.sort();

    let nothing: usize = 0x3f3f3f3f;
    let mut prev = nothing;
    let mut grouped = Vec::new();
    let mut tmp = Vec::new();
    for (x, c) in repr{
        if x != prev && prev != nothing{
            grouped.push(tmp);
            tmp = Vec::new();
        }
        prev = x;
        tmp.push(c);
    }
    grouped.push(tmp);

    for cs in grouped{
        let l = cs.len();
        let mut cs = cs.clone();
        cs.sort();
        let mut max_len = 0;
        let mut prev = nothing;
        let mut len = 0;
        for c in cs{
            if prev != c && prev != nothing{
                max_len = max(max_len, len);
                len = 0;
            }
            prev = c;
            len += 1;
        }
        max_len = max(max_len, len);
        res += l - max_len;
    }

    println!("{}", res);
}

struct UnionFind{
    parent: Vec<usize>,
    rank: Vec<usize>
}

impl UnionFind{
    fn new(n: usize) -> UnionFind{
        let mut parent: Vec<usize> = Vec::with_capacity(n);
        for i in 0..n{
            parent.push(i)
        }
        UnionFind{parent: parent, rank: vec![0; n]}
    }

    fn find(&mut self, x: usize) -> usize{
        if self.parent[x] == x {
            x
        } else {
            let px = self.parent[x];
            let root = self.find(px);
            self.parent[x] = root;
            root
        }
    }

    fn unite(&mut self, x: usize, y: usize){
        let px = self.find(x);
        let py = self.find(y);

        if px == py {return}

        if self.rank[px] < self.rank[py] {
            self.parent[px] = py;
        } else {
            self.parent[py] = px;
            if self.rank[px] == self.rank[py] {
                self.rank[px] += 1;
            }
        }

    }

    #[allow(dead_code)]
    fn equiv(&mut self, x: usize, y: usize) -> bool{
        self.find(x) == self.find(y)
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