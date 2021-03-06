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
    guru_tube_id = NA,
    guru_tissue_id = NA,
    guru_box_id = NA,
    in_guru = NA,
    is_depleted = NA,
    box_number = NA,
    box_row = NA,
    box_col = NA,
    timepoint = NA,
    sample_type = NA,


    # initialize an aliquot object from a database row
    initialize = function(db_row) {
      if (!missing(db_row))  {
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
        self$guru_tube_id <- db_row[['guru_tube_id']]
        self$guru_box_id <- db_row[['guru_box_id']]
        self$guru_tissue_id <- db_row[['guru_tissue_id']]
        self$sample_type <- db_row[['sample_type']]
      } else {
        warning('No database row passed, creating empty object...')
      }
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

  # not storing these now -- plate layout is stored in plate table

#   qstring <- paste0('update aliquot set plate_row = ', wrap(self$plate_row),
#                     ' where id=', self$id, ';')
#   dbSendQuery(db_con, qstring)
#
#   qstring <- paste0('update aliquot set plate_col = ', self$plate_col,
#                     ' where id=', self$id, ';')
#   dbSendQuery(db_con, qstring)

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


# generic interface for updating any column for a given aliquot in the database
Aliquot$set("public", "update_value_in_db", function(db_con, column_name,
                                                     value) {
  check_db(db_con)
  if (is.character(value)) value <- wrap(value) # chars need to be wrapped

  qstring <- paste0('update aliquot set ', column_name, ' = ', value,
                    ' where id=', self$id, ';')
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


# returns the box location as a single string
Aliquot$set("public", "get_loc", function() {
  return(paste0(self$box_row, self$box_col))
})


# TODO
# returns the box location as a single string
Aliquot$set("public", "get_guru_locstring", function() {

#   tlu <- setNames(c('cytokine', 'pbmc', 'neutrophil'), c('CT', 'PB', 'NT'))
#   tstr <- strsplit(self$barcode, '-')[[1]][2]
#   stype <- ifelse(is.na(tstr), 'unknown', tlu[tstr])
#   tube_descr <- paste(
#     paste("Sample Type:", stype),
#     paste("Patient Type:", ifelse(
#       grepl("CTRL", ali$barcode), 'control',
#       'patient'
#     )),
#     paste("Collection Timepoint:", ali$timepoint),
#     sep = "<br/>"
#   )

})






