#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let listener = std::net::TcpListener::bind("0.0.0.0:8000")?;
    newsletter::run(listener)?.await
}
