fn main(){
    let n = get_int() as usize;
    let xs = get_ints();

    let mut ixs: Vec<(usize, i64)> = (1..n+1).zip(xs).collect();
    ixs.sort_by(|&(_,x), &(_,y)|y.cmp(&x));

    let res: Vec<String> = ixs.iter().map(|&(i,_)|i.to_string()).collect();

    println!("{}", res.join("\n"));

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