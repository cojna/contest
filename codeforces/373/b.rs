#[allow(unused_imports)]
use std::cmp::{min, max};

const RED: isize = 0;
const BLACK: isize = 1;

fn main(){
    let _: i64 = readln();
    let cs: Vec<char> = get_line().chars().collect();

    println!("{}", min(solve(&cs, RED), solve(&cs, BLACK)));

}

fn solve(cs: &Vec<char>, first: isize) -> i64{
    let mut res = 0;
    let mut red = 0;
    let mut black = 0;
    let mut next = first;
    for &c in cs{
        if next == RED && c == 'b' {
            if red > 0 {
                red = red - 1;
            } else {
                res += 1;
                black += 1;
            }
        }

        if next == BLACK && c == 'r' {
            if black > 0 {
                black = black - 1;
            } else {
                res += 1;
                red += 1;
            }
        }

        next = 1 - next;
    }
    res
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