use std::net::TcpListener;
use secrecy::ExposeSecret;

use newsletter::{configuration::DatabaseSettings, telemetry::{get_subscriber, init_subscriber}};
use sqlx::{Connection, Executor, PgConnection, PgPool};
use uuid::Uuid;
use once_cell::sync::Lazy;

pub struct TestApp {
    pub address: String,
    pub db_pool: PgPool,
}

static TRACING: Lazy<()> = Lazy::new(|| {
    let subscriber = get_subscriber("test".into(), "debug".into());
    init_subscriber(subscriber);
});

pub async fn configure_database(configuration: &DatabaseSettings) -> PgPool {
    let mut connection = PgConnection::connect(&configuration.connection_string_without_db().expose_secret())
        .await
        .expect("Failed to connect to Postgres.");
    connection
        .execute(&*format!(
            r#"CREATE DATABASE "{}";"#,
            configuration.database_name
        ))
        .await
        .expect("Failed to create database.");

    let connection_pool = PgPool::connect(&configuration.connection_string_without_db().expose_secret())
        .await
        .expect("Failed to connect to Postgres.");
    sqlx::migrate!("./migrations")
        .run(&connection_pool)
        .await
        .expect("Failed to migrate database.");

    connection_pool
}

async fn spawn_app() -> TestApp {
    Lazy::force(&TRACING);

    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind random port.");
    let port = listener.local_addr().unwrap().port();
    let mut configuration =
        newsletter::configuration::get_configuration().expect("Failed to read configuration.");
    configuration.database.database_name = Uuid::new_v4().to_string();
    let connection_pool = configure_database(&configuration.database).await;
    let server = newsletter::startup::run(listener, connection_pool.clone())
        .expect("Failed to bind address.");

    println!("Server running on http://");

    let _ = tokio::spawn(server);

    TestApp {
        address: format!("http://localhost:{}", port),
        db_pool: connection_pool,
    }
}

#[tokio::test]
async fn test_health_check() {
    let app = spawn_app().await;

    let client = reqwest::Client::new();

    let response = client
        .get(&format!("{}/health_check", &app.address))
        .send()
        .await
        .expect("Failed to execute request.");
    assert!(response.status().is_success());
    assert_eq!(Some(0), response.content_length());
}

#[tokio::test]
async fn subscribe_returns_a_200_for_valid_form_data() {
    let app = spawn_app().await;
    let client = reqwest::Client::new();

    let body = "name=le%20guin&email=ursula_le_guin%40gmail.com";
    let response = client
        .post(&format!("{}/subscriptions", &app.address))
        .header("Content-Type", "application/x-www-form-urlencoded")
        .body(body)
        .send()
        .await
        .expect("Failed to execute request.");

    assert_eq!(200, response.status().as_u16());

    let saved = sqlx::query!("SELECT email, name FROM subscriptions",)
        .fetch_one(&app.db_pool)
        .await
        .expect("Failed to fetch saved subscription.");

    assert_eq!(saved.email, "ursula_le_guin@gmail.com");
    assert_eq!(saved.name, "le guin");
}

#[tokio::test]
async fn subscribe_returns_a_400_when_data_is_missing() {
    let app = spawn_app().await;
    let client = reqwest::Client::new();
    let test_cases = vec![
        ("name=le%20guin", "missing the email"),
        ("email=ursula_le_guin%40gmail.com", "missing the name"),
        ("", "missing both name and email"),
    ];

    for (invalid_body, error_message) in test_cases {
        let response = client
            .post(&format!("{}/subscriptions", &app.address))
            .header("Content-Type", "application/x-www-form-urlencoded")
            .body(invalid_body)
            .send()
            .await
            .expect("Failed to execute request.");

        assert_eq!(
            400,
            response.status().as_u16(),
            "The API did not fail with 400 Bad Request when the payload was {}.",
            error_message
        );
    }
}
