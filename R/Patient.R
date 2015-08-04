#' @include Aliquot.R utils.R


#' @title Patient class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @description A class representation of a patient.
#'
#' @format \code{\link{Patient}} class generator
#'
#' @usage \code{pat = Patient$new()}
#'
#' @keywords data
#'
Patient <- R6::R6Class(
  "Patient",
  public = list(
    id = NA,
    redcap_id = NA,
    project_id = NA,
    aliquots = list(),
    is_complete_0 = NA,
    is_complete_48 = NA,
    is_complete_192 = NA,
    all_complete = NA,

    initialize = function(db_row) {
      if (!missing(db_row)) {
        self$id <- db_row[['id']]
        self$redcap_id <- db_row[['redcap_id']]
        self$project_id <- db_row[['project_id']]
        self$is_complete_0 <- as.logical(db_row[['is_complete_0']])
        self$is_complete_48 <- as.logical(db_row[['is_complete_48']])
        self$is_complete_192 <- as.logical(db_row[['is_complete_192']])
        self$all_complete <- as.logical(db_row[['all_complete']])
      } else {
        warning('No database row passed, creating empty object...')
      }
    },


    # getting around known issue: https://github.com/wch/R6/issues/51
    say_hi = function(x) {
      cat('I am patient ', self$redcap_id)
    }
  )
)


Patient$set("public", "get_aliquots_to_run", function(db_con) {

  if (self$all_complete == TRUE) return(list())

  aliquots <- RSQLite::dbGetQuery(
    conn = db_con,
    statement = paste0("select * from aliquot where patient_id = ", self$id, " and is_depleted = 0 and plate_id is null;")
  )

  if (!nrow(aliquots)) {
    return(list())
  }

  # create a list of aliquots from our query
  ali_list <- lapply(1:nrow(aliquots), function(x) {
    Aliquot$new(aliquots[x, ])
  })

  to_ret <- list()
  tps <- sapply(ali_list, function(x) x$timepoint)

  if (!self$is_complete_0 & any(tps == "0 Hour")) {
    sel <- which(tps == "0 Hour")
    if (length(sel) > 1) sel <- sample(sel, size = 1)
    to_ret <- c(to_ret, ali_list[[sel]])
  }

  if (!self$is_complete_48 & any(tps == "48 Hour")) {
    sel <- which(tps == "48 Hour")
    if (length(sel) > 1) sel <- sample(sel, size = 1)
    to_ret <- c(to_ret, ali_list[[sel]])
  }

  if (!self$is_complete_192 & any(tps == "8 Day")) {
    sel <- which(tps == "8 Day")
    if (length(sel) > 1) sel <- sample(sel, size = 1)
    to_ret <- c(to_ret, ali_list[[sel]])
  }

  return(to_ret)

})


# generic interface for updating any column for a given aliquot in the database
Patient$set("public", "update_value_in_db", function(db_con, column_name,
                                                     value) {
  check_db(db_con)
  if (is.character(value)) value <- wrap(value) # chars need to be wrapped

  qstring <- paste0('update plate set ', column_name, ' = ', value,
                    ' where id=', self$id, ';')
  dbSendQuery(db_con, qstring)
})




# generic interface for updating any column for a given aliquot in the database
Patient$set("public", "save_to_db", function(db_con) {
  check_db(db_con)

  db_vals <- c("redcap_id", "project_id", "is_complete_0", "tissue_0",
               "is_complete_48", "tissue_48", "is_complete_192", "tissue_192",
               "all_complete")

  obj_vals <- sapply(db_vals, function(x) {
    val <- self[[x]]
    if (is.null(val) | is.na(val)) {
      return('null')
    } else {
      return(val)
    }
  })

  query <- paste0(
    'INSERT INTO patient (',
    paste(names(obj_vals), collapse = ", "),
    ') VALUES (',
    paste(obj_vals, collapse = ", "),
    ');'
  )

  dbGetQuery(db_con, query)

  # we have auto incrementing keys so our recently inserted value will be our
  # maximum
  new_id <- dbGetQuery(db_con, 'select ifnull(max(id), 0) from patient;')
  self$id <- unname(unlist(new_id))


})









