
check_db <- function(x) {
  if (missing(x))
    stop('You must provide a database connection...')

  if (!is(x, "SQLiteConnection"))
    stop('This is not a SQLite database connection, try again...')
}

wrap <- function(x) paste0('"', x, '"')
wrap2 <- function(x) paste0("'", x, "'")


