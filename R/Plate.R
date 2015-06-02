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
    creation_date = NA,
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



# initialize plate object from database
Plate$set("public", "init_from_db", function(db_con, id) {

  # make sure db connection is right
  check_db(db_con)

  # confirm id passed is in the database
  if (missing(id)) stop('You must provide a plate ID to build object from DB...')

  pt <- dbGetQuery(db_con, 'select * from plate;')

  if (id %in% pt[['id']]) {

    # add plate specific data
    r <- pt[pt[['id']] == id, ]
    self$id <- r[['id']]
    self$creation_date <- r[['creation_date']]
    self$run_date <- r[['run_date']]
    self$is_processed <- r[['is_processed']]

    # add all aliquots and plate layout
    aliquots <- dbGetQuery(
      db_con,
      paste0("select * from aliquot where plate_id = ", self$id, ";")
    )

    self$aliquots <- lapply(1:nrow(aliquots), function(x) {
      Aliquot$new(aliquots[x, ])
    })

    for (i in 1:nrow(aliquots)) {
      self$layout[aliquots[['plate_row']][i], aliquots[['plate_col']][i]] <- aliquots[['barcode']][i]
    }

  } else {
    warning('ID is not in database... Not updating plate information...')
  }

})



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

    cur_pat <- Patient$new(patients[pat_counter, ])
    pat_al <- cur_pat$get_aliquots_to_run(db_con = db_con)

    if (length(pat_al)) {
      self$patients <- c(self$patients, cur_pat)
      self$aliquots <- c(self$aliquots, pat_al)
      n_added <- n_added + length(pat_al)
    }

    pat_counter <- pat_counter + 1

    if (pat_counter > nrow(patients)) {
      warning('Warning... not enough samples processed to fill plate. Returning partial plate layout...')
      break()
    }
  }

})


# creates a plate configuration using from the list of the plates samples
# also stores location info in each aliquot object that is contained in the
# parent plate object
Plate$set("public", "create_layout", function() {
  ord <- sample(1:length(self$aliquots))
  ctr <- 1
  for (i in 1:nrow(self$layout)) {
    for (j in 1:ncol(self$layout)) {
      if (is.na(self$layout[i, j]) & ctr <= max(ord)) {
        self$layout[i, j] <- self$aliquots[[ord[ctr]]]$barcode
        self$aliquots[[ord[ctr]]]$plate_row <- LETTERS[i]
        self$aliquots[[ord[ctr]]]$plate_col <- j
        ctr <- ctr + 1
      }
    }
  }
})


# writes the plate configuration back to the database
Plate$set("public", "save_configuration", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # first, assign a plate ID and save
  ins <- paste0(
    'INSERT INTO plate (creation_date, run_date, is_processed) VALUES (',
    wrap(Sys.Date()), ', NULL, 0);')

  dbGetQuery(db_con, ins)

  # we have auto incrementing keys so our recently inserted value will be our
  # maximum
  plate_id <- dbGetQuery(db_con, 'select ifnull(max(id), 0) from plate;')
  self$id <- unname(unlist(plate_id))

  # then we need to iterate through all the alilquots we have in this plate and
  # set their plate ID to match the current one
  for (a in self$aliquots) {
    a$plate_id <- self$id
    a$update_plate(db_con)
  }
})



# renders a data frame to be displayed in the web app or written to
Plate$set("public", "render_table", function(db_con) {
  to_ignore <- c('Standard', 'Blank', 'NBISC')
  cl <- self$layout
  for (i in 1:nrow(cl)) {
    for (j in 1:ncol(cl)) {
      if (is.na(cl[i, j])) cl[i, j] <- 'Missing'
      else if (!(cl[i, j] %in% to_ignore)) {
        asel <- which(sapply(self$aliquots, function(x) x$barcode) == cl[i, j])
        if (length(asel)) cl[i, j] <- self$aliquots[[asel]]$get_locstring()
        else print(paste0('No match for ', cl[i, j]))
      }
    }
  }
  return(cl)
})


# sets a plate to be 'complete'
Plate$set("public", "set_complete", function(db_con, run_date = NULL) {

  # make sure db connection is right
  check_db(db_con)

  if (is.null(run_date)) run_date <- Sys.Date()

  self$run_date <- run_date
  self$is_processed <- 1

  # mark all of our aliquots as complete too
  lapply(self$aliquots, function(x) x$set_complete(db_con))


})




Plate$set("public", "demo", function(db_con) {

  library(BioradConfig)
  sqlite <- DBI::dbDriver("SQLite")
  dbname <- "~/Google Drive/spectrum/ec2-apps/internal-site/plate/data/test.db"
  db_con <- RSQLite::dbConnect(sqlite, dbname)

  # create a dummy plate configuration
  p <- Plate$new()
  p$get_aliquots(db_con)
  p$create_layout()

  # save config back to database to test
  p$save_configuration(db_con)

  p$render_table()

  # re-create the same cofiguration from the database
  p2 <- Plate$new()
  p2$init_from_db(db_con, 1)

  identical(p$layout, p2$layout)

  # mark plate as completed
  p$set_complete(db_con)


})

