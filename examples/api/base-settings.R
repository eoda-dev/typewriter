Sys.setenv(POSTGRES_USERNAME = "postgres")
Sys.setenv(POSTGRES_PASSWORD = "superSecret!")
Sys.setenv(POSTGRES_PORT = 15432)

postgres_settings <- base_settings(
  username = as.character,
  password = as.character,
  port = as.integer,
  .prefix = "POSTGRES"
)

# Succeeds
postgres_settings()

Sys.setenv(POSTGRES_PORT = "")

# Fails
try(postgres_settings())
