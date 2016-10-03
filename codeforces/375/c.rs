#[allow(unused_imports)]
use std::cmp::*;
#[allow(unused_imports)]
use std::collections::*;

fn main(){
    let (n, m): (usize, usize) = readln();
    let mut xs: Vec<usize> = readln();
    let mut freq: Vec<usize> = vec![0; m+1];
    let q = n / m;

    for &x in &xs{
        if x <= m{
            freq[x] += 1;
        }
    }

    let mut res = 0;

    for i in 1..m+1{
        while freq[i] < q{
            let mut found = false;
            for j in 0..n{
                if xs[j] > m {continue;}
                if freq[xs[j]] > q + 1{
                    freq[i] += 1;
                    freq[xs[j]] -= 1;
                    xs[j] = i;
                    res += 1;
                    found = true;
                    break;
                }
            }

            if found {continue;}

            for j in 0..n{
                if xs[j] > m{
                    freq[i] += 1;
                    xs[j] = i;
                    res += 1;
                    found = true;
                    break;
                }
            }

            if found {continue;}

            for j in 0..n{
                if xs[j] > m {continue;}
                if freq[xs[j]] > q{
                    freq[i] += 1;
                    freq[xs[j]] -= 1;
                    xs[j] = i;
                    res += 1;
                    found = true;
                    break;
                }
            }

            assert!(found);
        }
    }

    println!("{} {}", q, res);
    let res: Vec<String> = xs.iter().map(|x|x.to_string()).collect();
    println!("{}", res.join(" "));

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