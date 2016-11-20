#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let (n, m): (usize, usize) = readln();
    let mut map: Vec<Vec<u8>> = Vec::with_capacity(n);
    for _ in 0..n{
        let tmp: Vec<u8> = readln();
        map.push(tmp);
    }
    let mut a: Vec<Vec<usize>> = Vec::with_capacity(n);
    for _ in 0..n{
        let zeros = vec![0;m];
        a.push(zeros);
    }

    for i in 0..n{
        let mut flag = false;
        for j in 0..m{
            if map[i][j] == 1{
                flag = true;
            }
            if flag && map[i][j] ==0 {
                a[i][j] += 1;
            }
        }
    }

    for i in 0..n{
        let mut flag = false;
        for j in (0..m).rev(){
            if map[i][j] == 1{
                flag = true;
            }
            if flag && map[i][j] ==0 {
                a[i][j] += 1;
            }
        }
    }

    for j in 0..m{
        let mut flag = false;
        for i in 0..n{
            if map[i][j] == 1{
                flag = true;
            }
            if flag && map[i][j] ==0 {
                a[i][j] += 1;
            }
        }
    }

    for j in 0..m{
        let mut flag = false;
        for i in (0..n).rev(){
            if map[i][j] == 1{
                flag = true;
            }
            if flag && map[i][j] ==0 {
                a[i][j] += 1;
            }
        }
    }

    let mut res = 0;
    for i in 0..n{
        for j in 0..m{
            res += a[i][j];
        }
    }

    println!("{}",res);
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