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
    plate_row = NA,
    plate_col = NA,
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
      self$plate_row <- db_row[['plate_row']]
      self$plate_col <- db_row[['plate_col']]
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


# updates an aliquots plate assignment in the database
Aliquot$set("public", "update_plate", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # set plate id and location
  qstring <- paste0('update aliquot set plate_id = ', self$plate_id,
                    ' where id=', self$id, ';')
  dbSendQuery(db_con, qstring)

  qstring <- paste0('update aliquot set plate_row = ', wrap(self$plate_row),
                    ' where id=', self$id, ';')
  dbSendQuery(db_con, qstring)

  qstring <- paste0('update aliquot set plate_col = ', self$plate_col,
                    ' where id=', self$id, ';')
  dbSendQuery(db_con, qstring)

})


# sets an aliquot as being complete; updates database entry for aliquot and
# propagates completion to parent patient
Aliquot$set("public", "set_complete", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # mark as depleted
  qstring <- paste0('update aliquot set is_depleted = 1 where id=', self$id, ';')
  dbSendQuery(db_con, qstring)
  self$is_depleted <- TRUE

  # update patient table
  t_field <- switch(self$timepoint,
                    '0 Hour' = 'is_complete_0',
                    '48 Hour' = 'is_complete_48',
                    '8 Day' = 'is_complete_192'
                    )

  qstring <- paste0('update patient set ', t_field, ' = 1 where id = ', self$patient_id, ';')
  dbSendQuery(db_con, qstring)

})




# creates a text representation of the aliquot to be displayed in tables
# rendered of the plate configuration. should contain information to help
# lab users locate the
Aliquot$set("public", "get_locstring", function() {
  return(
    paste0(
      self$barcode,
      '<br/>',
      self$box_number,
      ': ',
      self$box_row,
      self$box_col
    )
  )
})

