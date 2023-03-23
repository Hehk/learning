use std::fs::File;
use std::io::{self, Read};

fn main() {
    // panic!("Everything is fine!")
    read_file();
}

fn read_file() -> Result<String, io::Error> {
    let mut username = String::new();
    File::open("username.txt")?.read_to_string(&mut username)?;
    Ok(username)
}
