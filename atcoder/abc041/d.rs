fn main(){
    let (n, m) = get_int2();
    let n = n as usize;
    let m = m as usize;
    let mut prev: Vec<usize> = vec![0;n];
    for _ in 0..m{
        let (x, y) = get_int2();
        let x = x as usize - 1;
        let y = y as usize - 1;
        prev[y] |= 1<<x;
    }
    let prev = prev;

    let mut dp: Vec<i64> = vec![0;1<<n];
    dp[0] = 1;
    for s in 0..1<<n{
        for i in 0..n{
            if s & (1<<i) != 0 && prev[i] & s == prev[i]{
                dp[s] += dp [s^(1<<i)];
            }
        }
    }

    println!("{}", dp[(1<<n)-1]);
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