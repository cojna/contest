#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let (w, h): (usize, usize) = readln();
    let mut pq: Vec<(usize, bool)> = Vec::with_capacity(w + h);
    for _ in 0..w{
        let p = readln();
        pq.push((p, true));
    }
    for _ in 0..h{
        let q = readln();
        pq.push((q, false));
    }

    pq.sort();

    let mut res = 0;
    let mut p = 0;
    let mut q = 0;
    for &(c, is_p) in &pq{
        if p == w && q == h {
            break;
        }

        if is_p && p < w{
            res += c * (h + 1 - q);
            p += 1;
        }else if q < h{
            res += c * (w + 1 - p);
            q += 1;
        }
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