fn main(){
    let s: Vec<char> = get_string().chars().collect();
    let i = get_int() as usize;
    println!("{}",s[i-1]);
}

#[allow(dead_code)]
fn get_string() -> String{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf
}

#[allow(dead_code)]
fn get_int() -> i64{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().parse().unwrap()
}

#[allow(dead_code)]
fn get_ints() -> Vec<i64>{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.split_whitespace().map(|s| s.parse().unwrap()).collect()
}

#[allow(dead_code)]
fn get_int2() -> (i64, i64){
    let v = get_ints();
    (v[0], v[1])
}