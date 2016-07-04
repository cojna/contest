use std::collections::BinaryHeap;

fn main(){
    let (n, m) = get_int2();
    let mut heap = BinaryHeap::with_capacity(m as usize);
    for _ in 0..m {
        let (a, b, y) = get_int3();
        heap.push((y, (a-1) as usize, (b-1) as usize));
    }
    let q = get_int() as usize;
    let mut wvjs = Vec::with_capacity(q);
    for j in 0..q{
        let (v, w) = get_int2();
        wvjs.push((w, (v-1) as usize, j));
    }
    wvjs.sort_by(|x, y| y.cmp(x));
    let mut uf = UnionFind::new(n as usize);
    let mut table = vec![0; q];

    for (w, v, j) in wvjs{
        loop{
            match heap.peek().cloned(){
                Some((y, a, b)) if y > w => {
                    uf.unite(a, b);
                    heap.pop();
                  },
                _ => break
            }
        }

        table[j] = uf.count(v);
    }

    let res: Vec<String> = table.iter().map(|x|x.to_string()).collect();

    println!("{}", res.join("\n"));

}

struct UnionFind{
    parent: Vec<usize>,
    rank: Vec<usize>,
    size: Vec<usize>
}

impl UnionFind{
    fn new(n: usize) -> UnionFind{
        let mut parent: Vec<usize> = Vec::with_capacity(n);
        for i in 0..n{
            parent.push(i)
        }
        UnionFind{parent: parent, rank: vec![0; n], size: vec![1; n]}
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
            self.size[py] += self.size[px];
        } else {
            self.parent[py] = px;
            self.size[px] += self.size[py];
            if self.rank[px] == self.rank[py] {
                self.rank[px] += 1;
            }
        }

    }

    #[allow(dead_code)]
    fn equiv(&mut self, x: usize, y: usize) -> bool{
        self.find(x) == self.find(y)
    }

    #[allow(dead_code)]
    fn count(&mut self, x: usize) -> usize{
        let root = self.find(x);
        self.size[root]
    }
}

#[allow(dead_code)]
fn get_int() -> i64{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().parse().unwrap()
}

#[allow(dead_code)]
fn get_ints() -> Vec<i64>{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.split_whitespace().map(|s| s.parse().unwrap()).collect()
}

#[allow(dead_code)]
fn get_int2() -> (i64, i64){
    let v = get_ints();
    (v[0], v[1])
}

#[allow(dead_code)]
fn get_int3() -> (i64, i64, i64){
    let v = get_ints();
    (v[0], v[1], v[2])
}