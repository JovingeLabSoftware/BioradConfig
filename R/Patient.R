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

