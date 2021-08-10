case class PostgresConfig(
    host: String,
    port : Int,
    user : String,
    password: Option[String],
    database: String,
    max: Int
)

val config = PostgresConfig(
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = Some("secret"),
  database = "fts",
  max = 10
)
