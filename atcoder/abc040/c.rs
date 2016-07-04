fn main(){
    let inf: i64 = 0x3f3f3f3f;
    let mut buf0 = String::new();
    let mut buf1 = String::new();

    std::io::stdin().read_line(&mut buf0).ok().unwrap();
    let n = buf0.trim().parse::<usize>().ok().unwrap();

    std::io::stdin().read_line(&mut buf1).ok().unwrap();
    let xs: Vec<i64> = buf1.split_whitespace().map(|s| s.parse::<i64>().ok().unwrap()).collect();

    let mut dp = [inf; 100001];

    dp[0] = 0;
    dp[1] = (xs[1]-xs[0]).abs();
    for i in 2..n{
        dp[i] = std::cmp::min((xs[i] - xs[i-1]).abs() + dp[i-1], (xs[i] - xs[i-2]).abs() + dp[i-2]);
    }

    println!("{}",dp[n-1]);
}