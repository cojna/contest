#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let n: usize = readln();
    let mut heap = BinaryHeap::new();
    let mut sorted = Vec::new();
    let (t, _): (i64, i64) = readln();
    let mut baloon = t;
    for _ in 1..n{
        let (t, w): (i64, i64) = readln();
        if t > baloon{
            heap.push(-(w-t));
        } else {
            sorted.push((-t, -w));
        }
    }
    sorted.sort();
    let mut res = heap.len();
    let len = sorted.len();
    let mut cur = 0;
    loop{
        match heap.peek(){
            Some(&w) => {
                let w = -w;
                if w < baloon {
                    heap.pop();
                    baloon -= w + 1;
                    while cur < len{
                        let t = -sorted[cur].0;
                        if t > baloon{
                            let w = -sorted[cur].1;
                            heap.push(-(w-t));
                            cur += 1;
                        } else {
                            break
                        }
                    }
                    res = min(res, heap.len());
                } else {
                    break
                }
            },
            None => break
        }
    }

    println!("{}", res + 1);
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