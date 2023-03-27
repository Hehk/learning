fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let largest = largest(&number_list);

    println!("The largest number is {}", largest);

    generic_struct();
}

fn largest<T>(list: &[T]) -> T
where
    T: PartialOrd + Copy,
{
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}

fn generic_struct() {
    println!("Generic struct");
    let integer = Point { x: 5, y: 10 };
    let float = Point { x: 1.0, y: 4.0 };

    println!("integer.x = {}", integer.x());
    println!("float.x = {}", float.x());
}
