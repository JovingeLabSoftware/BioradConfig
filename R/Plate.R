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
      self$layout[1:2, 3:5] <- 'NBISC'
      self$layout[8, 3:5] <- 'Blank'
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

    self$layout <- jsonlite::fromJSON(r[['layout']])

#     for (i in 1:nrow(aliquots)) {
#       self$layout[aliquots[['plate_row']][i], aliquots[['plate_col']][i]] <- aliquots[['id']][i]
#     }

  } else {
    warning('ID is not in database... Not updating plate information...')
  }

})



Plate$set("public", "get_aliquots", function(db_con) {

  # gets the aliquots needed to run our plate:
  #     - we randomly select 5 available aliquots to be run in triplicate
  #         - and enforce that none of these are the same patient
  #     - all other aliquots are run in triplicate
  #     - each aliquot to be run is stored in Plate$aliquots
  #     - aliquots are placed on plate via Plate$create_layout()

  # make sure db connection is right
  check_db(db_con)

  # number of triplicates and duplicates on each plate
  n_trips <- 5
  n_dups <- 28

  # grab a list of all patients with samples to run and randomize
  pq <- "select * from patient where all_complete = 0;"
  patients <- RSQLite::dbGetQuery(conn = db_con, statement = pq)
  patients <- patients[sample(nrow(patients)), ]

  # get all our aliquots!
  pat_counter <- 1
  trips_added <- 0
  dups_added <- 0

  while (trips_added < n_trips | dups_added < n_dups) {
    trip_run <- FALSE # has this sample had a triplicate run?
    cur_pat <- Patient$new(patients[pat_counter, ])
    pat_al <- cur_pat$get_aliquots_to_run(db_con = db_con)

    # if we need to add less aliquots than we just pulled, then only keep
    # how many we need; only need to check our duplicate count here b/c we
    # always fill triplicates first
    if (dups_added + length(pat_al) > n_dups) {
      to_keep <- n_dups - dups_added
      pat_al <- pat_al[1:to_keep]
    }

    if (length(pat_al)) {
      for (i in seq_along(pat_al)) {
        if (trips_added < n_trips & !trip_run) {
          add_ct <- 3
          trips_added <- trips_added + 1
          trip_run <- TRUE
        } else {
          add_ct <- 2
          dups_added <- dups_added + 1
        }
        for (j in 1:add_ct)
          self$aliquots <- c(self$aliquots, pat_al[[i]])
      }
      self$patients <- c(self$patients, cur_pat)
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
        self$layout[i, j] <- self$aliquots[[ord[ctr]]]$id
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
    "INSERT INTO plate (creation_date, run_date, is_processed, layout) VALUES (",
    wrap2(Sys.Date()), ", NULL, 0, '", jsonlite::toJSON(self$layout), "');")

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
Plate$set("public", "render_table", function() {
  to_ignore <- c('Standard', 'Blank', 'NBISC')
  cl <- self$layout
  for (i in 1:nrow(cl)) {
    for (j in 1:ncol(cl)) {
      if (is.na(cl[i, j])) cl[i, j] <- 'Missing'
      else if (!(cl[i, j] %in% to_ignore)) {
        asel <- which(sapply(self$aliquots, function(x) x$id) == cl[i, j])
        if (length(asel)) cl[i, j] <- self$aliquots[[asel[1]]]$get_locstring()
        else print(paste0('No match for ', cl[i, j]))
      }
    }
  }
  return(cl)
})


# returns a data frame mapping
Plate$set("public", "get_plate_order", function() {

  sample_order <- data.frame(Sample_Number = 1:71, Aliquot = NA)
  to_ignore <- c('Standard', 'Blank', 'NBISC')

  ctr <- 1
  cl <- self$layout
  for (i in 1:nrow(cl)) {
    for (j in 1:ncol(cl)) {
      if (is.na(cl[i, j])) {
        sample_order[ctr, 2] <- 'Missing'
        ctr <- ctr + 1
      } else if (!(cl[i, j] %in% to_ignore)) {
        asel <- which(sapply(self$aliquots, function(x) x$id) == cl[i, j])
        if (length(asel)) {
          sample_order[ctr, 2] <- paste0(self$aliquots[[asel[1]]]$id,' (',
                                         self$aliquots[[asel[1]]]$barcode, ')')
          ctr <- ctr + 1
        }
        else {
          stop(paste0('No match for ', cl[i, j]))
        }
      }
    }
  }

  return(sample_order)

})




# returns a list of data to be written to an excel sheet using
# openxlsx::write.xlsx()
Plate$set("public", "get_excel_data", function() {

  # figure out which boxes the samples are stored in
  boxes <- unique(sapply(self$aliquots, function(x) x$box_number))

  # create a cleaned version of render_table() + add box info
  ly <- self$render_table()
  ly[] <- lapply(ly, function(x) gsub('<br/>', '\r\n', x))
  new_data <- data.frame(matrix('', ncol = 12, nrow = length(boxes) + 3))
  colnames(new_data) <- colnames(ly)
  new_data[,1] <- c('', '', 'Boxes to pull:', sort(boxes))
  dd <- rbind(ly, new_data)

  # get the order of the samples on the plate
  ord <- self$get_plate_order()

  to_ret <- list(
    Configuration = dd,
    Plate_Order = ord
  )

  return(to_ret)
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
  p2$init_from_db(db_con, p$id)

  identical(p$layout, p2$layout)
  identical(p$render_table(), p2$render_table())

  # mark plate as completed
  p$set_complete(db_con)


})

