#[allow(unused_imports)]
use std::cmp::{min, max};

fn main(){

    let cs: Vec<char> = get_line().chars().collect();
    let len = cs.len();

    if len < 26 {
        println!("-1");
        return;
    }

    for i in 0..len-25 {
        let mut freq: Vec<usize> = vec![0;26];
        for j in i..i+26 {
            if cs[j] == '?' {
                continue;
            }
            let c = cs[j] as usize - 'A' as usize;
            freq[c] += 1;
        }

        if freq.iter().any(|&x: &usize| x > 1) {
            continue;
        }

        for j in 0..i {
            print!("{}", if cs[j] == '?' { 'A' } else { cs[j] })
        }

        for j in i..i+26 {
            if cs[j] != '?' {
                print!("{}", cs[j]);
            } else {
                for k in 0..26 {
                    if freq[k] == 0 {
                        print!("{}", (k as u8 + 'A' as u8) as char);
                        freq[k] = 1;
                        break;
                    }
                }
            }
        }

        for j in i+26..len {
            print!("{}", if cs[j] == '?' { 'A' } else { cs[j] })
        }
        return;
    }

    println!("-1")
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