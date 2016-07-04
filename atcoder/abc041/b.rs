const MOD: i64 = 1_000_000_007;

fn main(){
    let v = get_ints();
    let a = v[0];
    let b = v[1];
    let c = v[2];

    println!("{}",a*b%MOD*c%MOD);
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