#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    newsletter::run()?.await
}
