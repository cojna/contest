fn main(){
    let str = get_line();
    let mut freq = [0; 256];

    for c in str.bytes(){
        freq[c as usize] += 1;
    }

    let mut odd = 0;
    for x in freq.iter(){
        if x % 2 == 1{
            odd += 1;
        }
    }

    let res = if odd <= 1 {
        str.len()
     } else {
         2 * ((str.len()-odd)/2/odd) + 1
     };

    println!("{}", res)
}

#[allow(dead_code)]
fn get_line() -> String{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().to_string()
}

#[allow(dead_code)]
fn read<T: std::str::FromStr>(s: &str) -> T{
    s.parse::<T>().ok().unwrap()
}