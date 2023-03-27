use std::env;
use std::error::Error;

struct Config {
    query: String,
    file_path: String,
    ignore_case: bool,
}

impl Config {
    fn build(mut args: impl Iterator<Item = string>) -> Result<Config, &'static str> {
        if args.len() < 3 {
            return Err("not enough arguments");
        }

        let query = match args.next() {
            Some(arg) => arg,
            None => return Err("Didn't get a query string"),
        };
        let file_path = match args.next() {
            Some(arg) => arg,
            None => return Err("Didn't get a file path"),
        };
        let ignore_case = env::var("IGNORE_CASE").is_ok();

        Ok(Config {
            query: query,
            file_path: file_path,
            ignore_case,
        })
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("args: {:?}", args);

    let config = Config::build(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        std::process::exit(1);
    });

    if let Err(err) = run(config) {
        eprintln!("Application error: {}", err);
        std::process::exit(1);
    }
}

fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    contents
        .lines()
        .filter(|line| line.contains(query))
        .collect()
}

pub fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let query = query.to_lowercase();
    let mut results = Vec::new();

    for line in contents.lines() {
        if line.to_lowercase().contains(&query) {
            results.push(line);
        }
    }

    results
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = std::fs::read_to_string(config.file_path)?;

    let results = if config.ignore_case {
        search_case_insensitive(&config.query, &contents)
    } else {
        search(&config.query, &contents)
    };

    for line in results {
        println!("{line}");
    }

    Ok(())
}
