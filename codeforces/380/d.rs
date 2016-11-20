#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let (n, a, b, k): (usize, isize, usize, usize) = readln();
    let mut cs = get_chars();

    let mut len = 0;
    let mut prev = 0;
    let mut res: Vec<usize> = Vec::new();
    for i in 0..n{
        if cs[i] == '1' {
            if len < b {
                for j in prev..i{
                    cs[j] = '1'
                }
            }
            len = 0;
            prev = i + 1;
            continue;
        }
        len += 1;
    }

    len = 0;
    prev = 0;
    let mut rest = a;
    for i in 0..n{
        if cs[i] == '1'{
            len = 0;
            prev = i + 1;
            continue;
        }
        len += 1;
        if len == b && rest > 0{
            for j in prev..(i+1){
                cs[j] = 'x'
            }
            rest -= 1;
            len = 0;
            prev = i + 1;
        }
    }

    len = 0;
    for i in (0..n).rev(){
        if cs[i] == 'x'{
            res.push(i);
            break;
        } else if cs[i] == '1'{
            len = 0;
            continue;
        }

        len += 1;
        if len == b{
            res.push(i);
            len = 0;
        }
    }

    println!("{}", res.len());
    for x in res{
        print!("{} ", x + 1);
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

impl<A: Read, B: Read, C: Read, D: Read> Read for (A, B, C, D){
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]), D::read(tokens[3]))
    }
}