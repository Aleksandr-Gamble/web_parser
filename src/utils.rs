use seahash::hash as shash;


pub fn hash_str_i32(string: &str) -> i32 {
    // hashes love u64 but Postgres loves i32
    let h = shash(&string.as_bytes());
    h as i32 // see https://stackoverflow.com/questions/28273169/how-do-i-convert-between-numeric-types-safely-and-idiomatically
}

