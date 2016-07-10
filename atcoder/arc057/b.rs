use std::cmp::{max, min};

const INF: i64 = 0x3f3f3f3f_3f3f3f3f;

fn main(){
    let (n, k) = get_int2();
    let n = n as usize;
    let mut a: Vec<i64> = Vec::with_capacity(n);
    for _ in 0..n {
        a.push(get_int());
    }
    let a = a;

    let mut sum: Vec<i64> = vec![0;n+1];
    for i in 0..n {
        sum[i+1] = sum[i] + a[i];
    }
    let sum = sum;

    if sum[n] == k {
        println!("1");
    } else {
        let mut dp: Vec<Vec<i64>> = vec![vec![INF;n+1];n+1];
        dp[0][0] = 0;
        for i in 0..n {
            for j in 0..n {
                if dp[i][j] == INF {
                    continue;
                }
                dp[i+1][j] = min(dp[i+1][j], dp[i][j]);

                let w = if sum[i] == 0 {
                            1
                        } else {
                            dp[i][j] * a[i] / sum[i] + 1
                        };
                if w <= a[i] {
                    dp[i+1][j+1] = min(dp[i+1][j+1], dp[i][j] + w);
                }
            }
        }

        let mut res = 0;
        for j in 0..n+1 {
            if dp[n][j] <= k {
                res = max (res, j)
            }
        }

        println!("{}", res);
    }


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