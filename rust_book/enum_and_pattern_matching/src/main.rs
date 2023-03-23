enum IpAddrKind {
    V4(String),
    V6(String),
}

enum IpAddrKind2 {
    V4(u8, u8, u8, u8),
    V6(String),
}

fn main() {
    let home = IpAddrKind2::V4(127, 0, 0, 1);
    let loopback = IpAddrKind2::V6(String::from("::1"));

    iterate_list(List::Cons(
        1,
        Box::new(List::Cons(2, Box::new(List::Cons(3, Box::new(List::Nil))))),
    ));
}

fn route(ip_kind: IpAddrKind) {}

fn playing_with_option() {
    let some_number = Some(5);
    let some_string = Some("a string");

    let absent_number: Option<i32> = None;

    let x: i8 = 5;
    let y: Option<i8> = Some(5);

    let sum = x + match y {
        Some(val) => val,
        None => 0,
    };
}

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter,
}
fn value_of_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter => 25,
    }
}

// A boy just wants his lists
enum List {
    Nil,
    Cons(i32, Box<List>),
}

fn iterate_list(list: List) {
    match list {
        List::Nil => (),
        List::Cons(head, tail) => {
            println!("{}", head);
            iterate_list(*tail);
        }
    }
}
