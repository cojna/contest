#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let (n, m): (usize, usize) = readln();
    let mut exists: Vec<bool> = vec![false;m];
    let mut uf = UnionFind::new(m);
    for _ in 0..n {
        let xs: Vec<usize> = readln();
        let k = xs[0];
        let x = xs[1] - 1;
        exists[x] = true;
        for i in 2..k+1 {
            exists[xs[i]-1] = true;
            uf.unite(x, xs[i]-1);
        }
    }

    let mut origin = 0;
    for i in 0..m {
        if exists[i] {
            origin = i;
            break;
        }
    }

    for i in 0..m {
        if uf.equiv(origin, i) || !exists[i]{
            continue;
        } else {
            println!("NO");
            return;
        }
    }
    println!("YES");
}


#[allow(dead_code)]
fn get_line() -> String{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_string()
}

#[allow(dead_code)]
fn get_chars() -> Vec<char>{
    get_line().chars().collect()
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