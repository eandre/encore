# Basic SQL database definitions

from encoredev.storage.sqldb import SQLDatabase

# Simple database with empty config
users_db = SQLDatabase("users", {})

# Database with migrations path
orders_db = SQLDatabase("orders", {
    "migrations": "./migrations",
})
