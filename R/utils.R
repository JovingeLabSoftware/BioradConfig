
check_db <- function(x) {
  if (missing(x))
    stop('You must provide a database connection...')

  if (!is(x, "SQLiteConnection"))
    stop('This is not a SQLite database connection, try again...')
}

wrap <- function(x) paste0('"', x, '"')
wrap2 <- function(x) paste0("'", x, "'")


get_tp <- Vectorize(function(field_name) {
  tu <- setNames(c('0 Hour', '48 Hour', '8 Day'), c("baseline", "48", "8"))
  return(unname(tu[strsplit(field_name, '_')[[1]][3]]))
})

get_base_bc <- function(full_barcode) {
  strsplit(full_barcode, '-')[[1]][1]
}
