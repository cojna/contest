#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let n = readln();
    let mut a: Vec<usize> = vec![0;max(10,n)+1];
    let mut acc = 0;
    let mut x = 0;
    let mut i = 1;
    while i <= n {
        if i <= acc {
            a[i] = x
        } else {
            x += 1;
            acc += x;
            a[i] = x;
        }
        i += 1;
    }
    a[1] = 1;
    a[2] = 2;
    a[3] = 2;
    a[4] = 3;

    let mut acc = n;
    let mut res = vec![a[n]];
    while acc != 1 && acc != 2 {
        acc = acc - a[acc];
        res.push(a[acc]);
    }

    for &x in &res {
        println!("{}",x);
    }
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
