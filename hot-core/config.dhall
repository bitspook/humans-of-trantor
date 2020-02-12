{ dbUrl = env:DB_URL ? "postgresql://hot:hot@localhost:5432/hot"
, port = env:PORT ? 7000
, jwtKeysPath = "jwt-keys.pem"
, migrationsDir = "migrations"
}
