#' @include Aliquot.R Patient.R utils.R

#' @title Plate class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @description A class representation of a Bio-Rad plate.
#'
#' @format \code{\link{Plate}} class generator
#'
#' @usage \code{plate = Plate$new()}
#'
#' @keywords data
#'
Plate <- R6::R6Class(
  "Plate",
  public = list(

    # public fields
    id = NA,
    patients = list(),
    aliquots = list(),
    layout = data.frame(
      matrix(NA, ncol = 12, nrow = 8)
    ),
    run_date = NA,
    is_processed = NA,
    results = list(),


    # public methods
    initialize = function() {
      colnames(self$layout) <- 1:12
      rownames(self$layout) <- LETTERS[1:8]
      self$layout[,1] <- 'Standard'
      self$layout[,2] <- 'Standard'
      self$layout[2:3,3:5] <- 'NBISC'
      self$layout[1,3:4] <- 'Blank'
    },

    # getting around known issue: https://github.com/wch/R6/issues/51
    say_hi = function(x) {
      cat('I am plate ', self$id)
    }
  )
)

Plate$set("public", "get_aliquots", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # figure out how many aliquots we need on the plate
  n_needed <- sum(apply(self$layout, 1, function(x) sum(is.na(x))))

  # grab a list of all patients who still need at least one sample run
  patients <- RSQLite::dbGetQuery(
    conn = db_con,
    statement = "select * from patient where all_complete = 0;"
  )

  n_added <- 0
  pat_counter <- 1
  while (n_added < n_needed) {
    print(pat_counter)

    cur_pat <- Patient$new(patients[pat_counter, ])
    pat_al <- cur_pat$get_aliquots_to_run(db_con = db_con)

    if (length(pat_al)) {
      self$patients <- c(self$patients, cur_pat)
      self$aliquots <- c(self$aliquots, pat_al)
      n_added <- n_added + length(pat_al)
    } else {
      print('no aliquots for')
      print(cur_pat)
    }

    pat_counter <- pat_counter + 1

    if (pat_counter > nrow(patients)) {
      warning('Warning... not enough samples processed to fill plate. Returning partial plate layout...')
      break()
    }
  }

})


# this creates a plate configuration using from the list of the plates samples
Plate$set("public", "create_layout", function() {

  ord <- sample(1:length(self$aliquots))

  ctr <- 1
  for (i in 1:nrow(self$layout)) {
    for (j in 1:ncol(self$layout)) {
      if (is.na(self$layout[i, j]) & ctr < max(ord)) {
        self$layout[i, j] <- self$aliquots[[ord[ctr]]]$barcode
        ctr <- ctr + 1
      }
    }
  }
})


# this writes the plate configuration back to the database
Plate$set("public", "save_configuration", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # # this is how you can update certain values in the database
  # dbSendQuery(db, "update aliquot set plate_id = '1' where barcode='BC00001-CT-01';")



})




Plate$set("public", "dummy", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  library(BioradConfig)
  sqlite <- DBI::dbDriver("SQLite")
  dbname <- "~/Google Drive/spectrum/ec2-apps/internal-site/plate/data/test.db"
  db_con <- RSQLite::dbConnect(sqlite, dbname)

  p <- Plate$new()
  p$get_aliquots(db_con)
  p$create_layout()

})


#
#
#
# dbSendQuery(db, "update aliquot set plate_id = '1' where barcode='BC00001-CT-01';")
#
#
# # creating aliquot table -------------------------------------------------------
#
# # build up our aliquot table and create it in the database
# ali_tab <- cbind.data.frame(
#   data.frame(id = rownames(dat)),
#   dat[,c("barcode", "record_id")],
#   data.frame(is_depleted = 0, plate_id = 0),
#   dat[,c("box", "row", "col", "variable")]
# )
#
# names(ali_tab) <- c("id", "barcode", "redcap_id", "plate_id", "is_depleted",
#                     "box_number", "row_letter", "col_number", "timepoint")
#
# # create the table and populate it
# dbWriteTable(db, 'aliquot', ali_tab, row.names = FALSE)
# dbListTables(db)
# dbGetQuery(db, "select * from aliquot")
#
# # this is how you can update certain values in the database
# dbSendQuery(db, "update aliquot set plate_id = '1' where barcode='BC00001-CT-01';")
#
#
#
#
# make_dummy_plate <- function() {
#   dummy_data <- data.frame(
#     matrix(NA, ncol = 12, nrow = 8)
#   )
#
#   colnames(dummy_data) <- 1:12
#   rownames(dummy_data) <- LETTERS[1:8]
#
#   dummy_data[,1] <- 'Standard'
#   dummy_data[,2] <- 'Standard'
#   dummy_data[2:3,3:5] <- 'NBISC'
#   dummy_data[1,3:4] <- 'Blank'
#
#
#   # populate this for now...
#   for (i in 1:nrow(dummy_data)) {
#     for (j in 1:ncol(dummy_data)) {
#       if (is.na(dummy_data[i, j])) {
#         dummy_data[i, j] <- make_bc_output(sample(dat$barcode, size = 1))
#       }
#     }
#   }
#
#   return(dummy_data)
# }
#
