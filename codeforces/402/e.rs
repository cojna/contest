#![allow(unused_imports)]
#![allow(dead_code)]
use std::cmp::*;
use std::collections::*;

fn main() {
    let (n, m) = readln();
    let mut env0: HashMap<String, Vec<bool>> = HashMap::with_capacity(n);
    let mut env1: HashMap<String, Vec<bool>> = HashMap::with_capacity(n);
    env0.insert("?".to_string(), vec![false;m]);
    env1.insert("?".to_string(), vec![true;m]);

    for _ in 0..n {
        let s = get_line();
        let strs: Vec<&str> = s.split_whitespace().collect();
        let v = strs[0].to_string();
        if strs.len() == 3 {
            let bits: Vec<bool> = strs[2].chars().map(|c| c == '1').collect();
            env0.insert(v.clone(), bits.clone());
            env1.insert(v, bits);
        } else {
            let l0 = env0.get(strs[2]).unwrap().clone();
            let r0 = env0.get(strs[4]).unwrap().clone();
            let l1 = env1.get(strs[2]).unwrap().clone();
            let r1 = env1.get(strs[4]).unwrap().clone();
            let mut bits0 = Vec::with_capacity(m);
            let mut bits1 = Vec::with_capacity(m);
            match strs[3] {
                "AND" => {
                    for i in 0..m {
                        bits0.push(l0[i] && r0[i]);
                        bits1.push(l1[i] && r1[i]);
                    }
                }
                "OR" => {
                    for i in 0..m {
                        bits0.push(l0[i] || r0[i]);
                        bits1.push(l1[i] || r1[i]);
                    }
                }
                "XOR" => {
                    for i in 0..m {
                        bits0.push(l0[i] != r0[i]);
                        bits1.push(l1[i] != r1[i]);
                    }
                }
                _ => unreachable!(),
            }
            env0.insert(v.clone(), bits0);
            env1.insert(v, bits1);
        }
    }

    let mut freq0: Vec<usize> = vec![0;m];
    let mut freq1: Vec<usize> = vec![0;m];
    for bits in env0.values() {
        for i in 0..m {
            if bits[i] {
                freq0[i] += 1;
            }
        }
    }
    for bits in env1.values() {
        for i in 0..m {
            if bits[i] {
                freq1[i] += 1;
            }
        }
    }
    for i in 0..m {
        freq1[i] -= 1;
    }

    let mut res_min: Vec<char> = Vec::with_capacity(m);
    let mut res_max: Vec<char> = Vec::with_capacity(m);
    for i in 0..m {
        match freq0[i].cmp(&freq1[i]) {
            Ordering::Less => {
                res_min.push('0');
                res_max.push('1')
            }
            Ordering::Equal => {
                res_min.push('0');
                res_max.push('0');
            }
            Ordering::Greater => {
                res_min.push('1');
                res_max.push('0')
            }
        }
    }

    let res_min: String = res_min.into_iter().collect();
    let res_max: String = res_max.into_iter().collect();
    println!("{}", res_min);
    println!("{}", res_max);

}


fn get_line() -> String {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_string()
}

fn get_chars() -> Vec<char> {
    get_line().chars().collect()
}

fn readln<T: Read>() -> T {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    T::read(buf.trim_right())
}

trait Read {
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

impl<T: Read> Read for Vec<T> {
    fn read(s: &str) -> Self {
        s.split_whitespace().map(T::read).collect()
    }
}

impl<A: Read, B: Read> Read for (A, B) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]))
    }
}

impl<A: Read, B: Read, C: Read> Read for (A, B, C) {
    fn read(s: &str) -> Self {
        let tokens: Vec<_> = s.split_whitespace().collect();
        (A::read(tokens[0]), B::read(tokens[1]), C::read(tokens[2]))
    }
}
