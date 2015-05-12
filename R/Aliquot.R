#' @include utils.R

#' @title Aliquot class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @description A class representation of a cytokine aliquot.
#'
#' @format \code{\link{Aliquot}} class generator
#'
#' @usage \code{al = Aliquot$new()}
#'
#' @keywords data
#'
Aliquot <- R6::R6Class(
  "Aliquot",
  public = list(
    id = NA,
    barcode = NA,
    redcap_id = NA,
    plate_id = NA,
    patient_id = NA,
    is_depleted = NA,
    box_number = NA,
    box_row = NA,
    box_col = NA,
    timepoint = NA,

    # initialize an aliquot object from a database row
    initialize = function(db_row) {
      if (missing(db_row)) stop('You must provide a database row...')
      self$id <- db_row[['id']]
      self$barcode <- db_row[['barcode']]
      self$plate_id <- db_row[['plate_id']]
      self$patient_id <- db_row[['patient_id']]
      self$is_depleted <- as.logical(db_row[['is_depleted']])
      self$box_number <- db_row[['box_number']]
      self$box_row <- db_row[['box_row']]
      self$box_col <- db_row[['box_col']]
      self$timepoint <- db_row[['timepoint']]
    },

    # getting around known issue: https://github.com/wch/R6/issues/51
    say_hi = function(x) {
      cat('I am aliquot ', self$id)
    }

  )
)



# sqlite> PRAGMA table_info(aliquot);
# 0|id|TEXT|0||0
# 1|barcode|TEXT|0||0
# 2|redcap_id|TEXT|0||0
# 3|plate_id|REAL|0||0
# 4|is_depleted|REAL|0||0
# 5|box_number|TEXT|0||0
# 6|row_letter|TEXT|0||0
# 7|col_number|REAL|0||0
# 8|timepoint|TEXT|0||0
