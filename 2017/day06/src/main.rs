fn main() {
    let mut mem_bank = [4,10,4,1,8,4,9,14,5,1,14,15,0,15,3,5];
    let mut configurations:Vec<String> = Vec::with_capacity(10000);
    let mut index: i32;

    add_config(&mut configurations, mem_bank);

    loop {
        let start = max_index(mem_bank);
        let distribute = mem_bank[start];
        for i in 0..distribute {
            mem_bank[(start + 1 + i as usize) % 16] += 1;
        }
        mem_bank[start] -= distribute;
        index = add_config(&mut configurations, mem_bank);
        if index != -1 {
            break;
        }
    }
    println!("Part 1: {:?}", configurations.len());
    println!("Part 2: {:?}", configurations.len() as i32 - index);
}

fn max_index(v: [i32; 16]) -> usize {
    let mut ind: usize = 0;
    let mut max: i32 = 0;
    for i in 0..16 {
        if v[i] > max {
            ind = i;
            max = v[i];
        }
    }
    return ind;
}

fn add_config(confs: &mut Vec<String>, v: [i32; 16]) -> i32 {
    let textvec: Vec<String> = v.iter().map(|x| x.to_string()).collect();
    let conf = textvec.join(",");
    if confs.contains(&conf) {
        return confs.iter().position(|x| *x == conf).unwrap() as i32;
    }
    confs.push(conf.clone());
    return -1;
}