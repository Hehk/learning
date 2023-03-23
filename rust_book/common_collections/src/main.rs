fn main() {
    // let v: Vec<i32> = Vec::new();
    let v = vec![1, 2, 3];

    println!("VECTORS!!");
    println!("Updating Vector");
    update_vector();
    println!("");
    println!("Reading Elements of Vector");
    reading_elements_of_vector();
    println!("");
    println!("Iteration over a Vector");
    iteration_over_a_vector();

    println!("");
    println!("STRINGS!!");
    println!("");
    println!("Concat Strings");
    concat_strings();

    println!("");
    println!("HASH MAPS!!");
    println!("");
    println!("Creating a new Hash Map");
    create_hash_map();
    println!("");
    println!("Updating a Hash Map");
    update_hash_map();
}

fn update_vector() {
    let mut v = vec![1, 2, 3];

    v.push(4);
    v.push(5);
    println!("Updating Vector {:?}", v);
}

fn reading_elements_of_vector() {
    let v = vec![1, 2, 3, 4, 5];

    let third: &i32 = &v[2];
    println!("The third element is {}", third);

    match v.get(2) {
        Some(third) => println!("The third element is {}", third),
        None => println!("There is no third element."),
    }
}

fn iteration_over_a_vector() {
    let v = vec![100, 32, 57];
    for i in &v {
        println!("{}", i);
    }
}

fn concat_strings() {
    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2;

    println!("{}", s3);
}

fn create_hash_map() {
    use std::collections::HashMap;

    let mut scores = HashMap::new();

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    println!("{:?}", scores);
}

fn update_hash_map() {
    use std::collections::HashMap;

    let mut scores = HashMap::new();

    // let blue = String::from("Blue");
    // testing ownership

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Blue"), 25);

    scores.entry(String::from("Yellow")).or_insert(50);
    scores.entry(String::from("Blue")).or_insert(50);

    println!("{:?}", scores);

    let text = "hello world wonderful world";
    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }
    println!("{:?}", map);
}
