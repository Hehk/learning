fn main() {
    let s = String::from("hello");
    takes_ownership(&s);
    println!("{}", s); // error[E0382]: borrow of moved value: `s`

    let x = 5;
    makes_copy(x);
    println!("{}", x);
}

// fn string_ownership() {
//     let s1 = String::from("hello");
//     let s2 = s1;

//     println!("{}, world!", s1);
// }

fn takes_ownership(some_string: &String) {
    println!("{}", some_string);
}

fn makes_copy(some_integer: i32) {
    println!("{}", some_integer);
}
