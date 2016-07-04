fn main(){
    let v: Vec<i64> = get_line().split_whitespace().map(read).collect();
    let h = v[0];
    let w = v[1];
    println!("{}",h*(w-1)+w*(h-1))
}

#[allow(dead_code)]
fn get_line() -> String{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf
}

#[allow(dead_code)]
fn read<T: std::str::FromStr>(s: &str) -> T{
    s.parse::<T>().ok().unwrap()
}