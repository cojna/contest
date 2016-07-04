fn main(){
    let mut buf = String::new();

    std::io::stdin().read_line(&mut buf).ok().unwrap();
    let n = buf.trim().parse::<i64>().ok().unwrap();

    let mut res = std::i64::MAX;
    for x in 1..1001{
        let y = n / x;
        let r = n - x * y;
        res = std::cmp::min(res, (x - y).abs() + r);
    }

    println!("{}", res);
}