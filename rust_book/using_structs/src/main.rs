fn main() {
    let user1 = User {
        email: String::from("test@example.com"),
        username: String::from("test"),
        active: true,
        sign_in_count: 1,
    };

    let user2 = User {
        email: String::from("test2@example.com"),
        ..user1
    };

    let rect = (30, 50);

    println!(
        "The area of the rectangle is {} square pixels.",
        area(rect.0, rect.1)
    );

    println!(
        "The area of the rectangle is {} square pixels.",
        area_2(rect)
    );

    let rect_2 = Rectangle {
        width: 30,
        height: 50,
    };

    println!(
        "The area of the rectangle is {} square pixels.",
        area_3(&rect_2)
    );

    println!("rect_2 is {:?}", rect_2);

    println!(
        "The area of the rectangle is {} square pixels.",
        rect_2.area()
    );
}

struct User {
    active: bool,
    username: String,
    email: String,
    sign_in_count: u64,
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn area(width: u32, height: u32) -> u32 {
    width * height
}

fn area_2(rect: (u32, u32)) -> u32 {
    rect.0 * rect.1
}

fn area_3(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}
