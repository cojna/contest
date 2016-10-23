#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

const INF: usize = 0x3f3f3f3f;

fn main(){
    let (h, w): (usize, usize) = readln();
    let mut c: Vec<Vec<char>> = Vec::with_capacity(h);
    for _ in 0..h {
        let cs = get_chars();
        c.push(cs);
    }

    let mut res = 0;
    for l in 0..w-1 {
        let r = l + 1;
        let mut dp: Vec<Vec<usize>> = vec![vec![INF;h+1];h+1];
        let mut cost: Vec<Vec<usize>> = vec![vec![0;h+1];h+1];
        for d in 0..h {
            cost[h-d][h] = 0;
            cost[h][h-d] = 0;
            for i in (0..h-d).rev() {
                cost[i][i+d] = cost[i+1][i+1+d];
                if c[h-1-i][l] == c[h-1-i-d][r] {
                    cost[i][i+d] += 1;
                }
                cost[i+d][i] = cost[i+1+d][i+1];
                if c[h-1-i-d][l] == c[h-1-i][r] {
                    cost[i+d][i] += 1;
                }
            }
        }

        dp[0][0] = 0;
        for i in 0..h+1 {
            for j in 0..h+1 {
                if i > 0 {
                    dp[i][j] = min(dp[i][j], dp[i-1][j] + cost[i-1][j]);
                }
                if j > 0 {
                    dp[i][j] = min(dp[i][j], dp[i][j-1] + cost[i][j-1]);
                }
            }
        }
        res += dp[h][h];
    }

    println!("{}", res);
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

#[derive(PartialEq, Eq)]
struct Down<T>(T);

impl<T> PartialOrd for Down<T> where T: PartialOrd{
    fn partial_cmp(&self, other: &Down<T>) -> Option<Ordering>{
        other.0.partial_cmp(&self.0)
    }
}

impl<T> Ord for Down<T> where T: Ord{
    fn cmp(&self, other: &Down<T>) -> Ordering{
        other.0.cmp(&self.0)
    }
}

type MaxHeap<T> = BinaryHeap<T>;
type MinHeap<T> = BinaryHeap<Down<T>>;
