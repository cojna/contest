#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

const NOTHING: usize = std::usize::MAX;

fn main() {
    let (n, m): (usize, usize) = readln();
    let mut es: Vec<(usize, usize)> = Vec::with_capacity(m);
    for _ in 0..m {
        let (a, b): (usize, usize) = readln();
        es.push((a - 1, b - 1));
    }
    let q: usize = readln();
    let mut qs: Vec<(usize, usize, usize)> = Vec::with_capacity(q);
    for _ in 0..q {
        let (x, y, z): (usize, usize, usize) = readln();
        qs.push((x - 1, y - 1, z));
    }

    let mut bit = m.next_power_of_two();
    let inf = 2 * bit - 1;
    let mut cand: Vec<(usize, usize)> = vec![inf;q]
        .into_iter()
        .enumerate()
        .map(|(x, y)| (y, x))
        .collect();
    while bit > 0 {
        cand.sort();
        let mut uf = UnionFind::new(n);
        let mut next: Vec<(usize, usize)> = Vec::with_capacity(q);
        let mut cur = 0;
        for i in 0..m {
            let (a, b) = es[i];
            uf.unite(a, b);
            while let Some(&(k, qi)) = cand.get(cur) {
                let k = k - bit;
                if m <= k {
                    next.push((k, qi));
                    cur += 1;
                    continue;
                }
                if i != k {
                    break;
                }
                let (x, y, z) = qs[qi];
                let px = uf.find(x);
                let py = uf.find(y);
                let mut ok = false;
                if px == py {
                    if uf.size(px) >= z {
                        ok = true;
                    }
                } else {
                    if uf.size(px) + uf.size(py) >= z {
                        ok = true;
                    }
                }

                if ok {
                    next.push((k, qi));
                } else {
                    next.push((k + bit, qi));
                }

                cur += 1;
            }
        }
        cand = next;
        bit /= 2;
    }

    let mut res: Vec<usize> = vec![NOTHING;q];
    for (k, i) in cand {
        res[i] = k;
    }
    let strs: Vec<String> = res.iter().map(|x| (x + 1).to_string()).collect();
    println!("{}", strs.join("\n"));
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
