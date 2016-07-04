fn main(){
    let mut buf = String::new();

    std::io::stdin().read_line(&mut buf).ok().unwrap();

    let xs: Vec<i64> = buf.split_whitespace().map(|s| s.parse::<i64>().ok().unwrap()).collect();
    let n = xs[0];
    let x = xs[1];

    let res = std::cmp::min (x - 1, n - x);

    println!("{}", res);
}