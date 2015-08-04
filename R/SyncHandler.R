#' @include utils.R redcap-utils.R guru-utils.R

#' @title SyncHandler class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @description A class to handle syncing our data in REDCap, DB, and LabGuru.
#'
#' @format \code{\link{SyncHandler}} class generator
#'
#' @usage \code{sync = SyncHandler$new()}
#'
#' @keywords data
#'

SyncHandler <- R6::R6Class(
  "SyncHandler",
  public = list(

    redcap_id = NA,
    redcap_project = NA,
    redcap_instrument = NA,
    redcap_token = NA,
    redcap_url = NA,
    guru_token = NA,
    redcap_data = NULL,
    db_data = NULL,
    local_id = NA,


    # a SyncHander should be initialized with data from a REDCap DET POST &
    # necessary API keys:
    # - project_id
    # - instrument
    # - record
    # - REDCap API key
    # - GuruApi Key

    initialize = function(project_id, instrument, redcap_id, redcap_token,
                          guru_token, redcap_url) {

      if (missing(project_id)) stop('You must provide a project ID...')
      if (missing(instrument)) stop('You must provide a REDCap instrument...')
      if (missing(redcap_id)) stop('You must provide a redcap_id...')
      if (missing(redcap_token)) stop('You must provide a redcap token...')
      if (missing(redcap_url)) stop('You must provide a redcap url...')
      if (missing(guru_token)) stop('You must provide a labguru token...')


      # first, check to see if our LabGuru token is stale
      if (!check_guru_key(guru_token)) {
        creds <- readRDS('~/.labguru/secrets.rds')
        guru_token <- get_guru_key(creds$username, creds$password)
      }

      self$redcap_id <- redcap_id
      self$redcap_project <- project_id
      self$redcap_instrument <- instrument
      self$redcap_token <- redcap_token
      self$redcap_url <- redcap_url
      self$guru_token <- guru_token
    },

    # getting around known issue: https://github.com/wch/R6/issues/51
    say_hi = function(x) {
      cat('I am SyncHandler ', paste(self$redcap_project, self$redcap_id, sep = '-'))
    }

  )

)



# updates an aliquots plate assignment in the database
SyncHandler$set("public", "get_redcap_data", function() {

  api_payload <- create_api_config(api_url = self$redcap_url,
                                   token = self$redcap_token)
  dd <- get_data_dict(api_config = api_payload)
  dd <- subset(dd, form_name == self$redcap_instrument)
  field_master <- c("record_id", grep("^bc", dd$field_name, value = TRUE))

  barcodes <- get_redcap_values(api_payload = api_payload,
                                fnames = field_master,
                                record_id = self$redcap_id)

  # stack this data frame -- kind of awkward
  starts <- seq(2, ncol(barcodes), 3)
  self$redcap_data <- do.call(rbind, lapply(starts, function(x) {
    cbind.data.frame(
      barcodes[ ,1],
      data.frame(bc_string = names(barcodes)[x], stringsAsFactors = FALSE),
      data.frame(
        barcode = barcodes[,x],
        box = barcodes[,x  + 1],
        location = barcodes[,x  + 2],
        stringsAsFactors = FALSE
      )
    )
  }))

})


# gets all the aliquots associated with this redcap patient
SyncHandler$set("public", "get_local_data", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # get all aliquots for that patient
  q <- paste0("select * from aliquot where patient_id = ", self$local_id)
  self$db_data <- dbGetQuery(db_con, q)

})



# checks to see that a patient is in the local databse and creates one if it
# is not
SyncHandler$set("public", "get_patient", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # find or create our patient
  q <- paste0("select id from patient where redcap_id = ", self$redcap_id,
              " and project_id = ", self$redcap_project)
  db_value <- unlist(dbGetQuery(db_con, q))

  if (length(db_value)) {
    self$local_id <- db_value
  } else {
    pat <- Patient$new()
    pat$redcap_id <- self$redcap_id
    pat$project_id <- self$redcap_project
    pat$is_complete_0 <- pat$is_complete_48 <- pat$is_complete_192 <- 0
    pat$save_to_db(db_con)
    self$local_id <- pat$id
  }
})


# confirms all the tissues for the necessary tissues are in LabGuru for the
# given barcodes; creates any missing tissues and adds them to the database
SyncHandler$set("public", "check_tissues", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # tissues associated with our patient
  tmp <- data.frame(
    timepoint = get_tp(self$redcap_data$bc_string),
    base_bc = sapply(strsplit(self$redcap_data$barcode, '-'), function(x) x[1]),
    stringsAsFactors = FALSE
  )
  tmp <- tmp[!is.na(tmp$base_bc),]
  tmp <- tmp[!duplicated(tmp), ]


  # add any tissues not currently in the database
  all_tissues <- dbGetQuery(db_con, 'select * from tissue;')
  miss_sel <- which(!(tmp$base_bc %in% all_tissues$labguru_name))

  if (length(miss_sel)) {
    for (i in seq_along(miss_sel)) {
      tissue_descr <- paste(
        paste("Patient Type:", ifelse(
          grepl("CTRL", tmp$base_bc[miss_sel[i]]), 'control',
          'patient'
        )),
        paste("Collection Timepoint:", tmp$timepoint[miss_sel[i]]),
        sep = "<br/>"
      )

      tissue_info <- create_tissue(base_id = tmp$base_bc[miss_sel[i]],
                                   token = self$guru_token,
                                   descr = tissue_descr)

      query <- paste0(
        'INSERT INTO tissue (labguru_id, labguru_name, labguru_uuid) VALUES (',
        paste(tissue_info['id'], wrap(tissue_info['name']),
              wrap(tissue_info['uuid']), sep = ", "),
        ');'
      )
      dbSendQuery(db_con, query)
    }
  }
})



# confirms all necessary boxes are in labguru; creates any that are missing
SyncHandler$set("public", "check_boxes", function(db_con) {

  check_db(db_con)
  all_boxes <- unique(self$redcap_data$box)
  tk <- is.na(all_boxes) | all_boxes == ''
  all_boxes <- all_boxes[!tk]
  db_boxes <- dbGetQuery(db_con, 'select * from box;')
  miss_sel <- which(!(all_boxes %in% db_boxes$labguru_name))
  if (length(miss_sel)) {
    for (i in seq_along(miss_sel)) {
      box_info <- create_box(box_name = all_boxes[miss_sel[i]],
                             token = self$guru_token)
      query <- paste0(
        'INSERT INTO box (labguru_id, labguru_name) VALUES (',
        paste(box_info['id'], wrap(box_info['name']), sep = ", "),
        ');'
      )
      dbSendQuery(db_con, query)
    }
  }
})



# creates a set of new aliquots in the database and in LabGuru
# new_idx is a vector giving the rows in self$redcap_data to create
SyncHandler$set("public", "create_new_aliquots", function(db_con, new_idx) {

  # build up a dataframe to dump in our database for all our new aliquots...
  new_alis <- data.frame(matrix(NA, ncol = ncol(self$db_data),
                                nrow = nrow(self$redcap_data)))
  names(new_alis) <- names(self$db_data)
  new_alis <- new_alis[,-1]
  new_alis$barcode <- self$redcap_data$barcode
  new_alis$timepoint <- get_tp(self$redcap_data$bc_string)
  new_alis$redcap_id <- self$redcap_id


  nrow(new_alis) <- nrow(self$redcap_data)



  inserts <- paste0(
    'INSERT INTO aliquot (',
    paste(names(ali_tab), collapse = ", "),
    ') VALUES (',
    apply(ali_tab, 1, function(x)
      paste(
        x[1], x[2], wrap(x[3]), x[4], x[5],
        wrap(x[6]), wrap(x[7]), x[8], wrap(x[9]),
        x[10], x[11], x[12], x[13], sep = ", "
      )),
    ');'
  )

  sapply(inserts, function(x)
    dbGetQuery(db, x)) # push all the aliquots in...




})


# updates an aliquots plate assignment in the database
SyncHandler$set("public", "sync_data", function(db_con) {

  check_db(db_con)

  # make sure our patient exists and set our local_id field telling us what the
  # ID of the patient in the database is
  self$get_patient(db_con)

  # get all the data we need from REDCap -- used for setting tissues
  self$get_redcap_data()

  # make sure all the tissues exist
  self$check_tissues(db_con)

  # make sure all the boxes exist
  self$check_boxes(db_con)




  # grab local data to compare with what is in REDCap
  self$get_local_data(db_con)






  # run through our values and see what is different or missing
  to_update <- c()
  to_add <- c()
  for (i in 1:nrow(self$redcap_data)) {
    sel <- which(self$db_data[['barcode']] == self$redcap_data[['barcode']][i])

    if (!length(sel)) {
      to_add <- c(to_add, i)
    }
    else if (length(sel) == 1) {

      al <- Aliquot$new(self$db_data[sel, ])
      bmatch <- al$box_number == self$redcap_data[['box']][i]
      lmatch <- al$get_loc() == self$redcap_data[['location']][i]
      if (!(bmatch & lmatch)) to_update <- c(to_update, i)

    } else {

      # figure out which one of the multiple ones is the real match
      al_match <- lapply(sel, function(x) Aliquot$new(self$db_data[x, ]))
      bmatch <- sapply(al_match, function(x) x$box_number) == self$redcap_data[['box']][i]
      lmatch <- sapply(al_match, function(x) x$get_loc()) == self$redcap_data[['location']][i]
      perf_match <- bmatch & lmatch

      # if none of the existing aliquots were perfect matches, then we need to
      # add a new one or update it -- how do we know!? i will default to adding it for now
      if (sum(perf_match) == 0) {
        to_add <- c(to_add, i)
      } else if (sum(perf_match) > 1) {
        print('This row matches multiple entries in our current database:')
        print(self$redcap_data[i, ])
        print('')
        stop('This should not be possible...')

      }
    }
  }

  # add all the new aliquots
  self$create_new_aliquots(db_con, to_add)



  new_alis <- self$db_data[0, ]



  # we have auto incrementing keys so our recently inserted value will be our
  # maximum
  plate_id <- dbGetQuery(db_con, 'select ifnull(max(id), 0) from plate;')
  self$id <- unname(unlist(plate_id))


  inserts <- paste0(
    'INSERT INTO aliquot (',
    paste(names(ali_tab), collapse = ", "),
    ') VALUES (',
    apply(ali_tab, 1, function(x)
      paste(
        x[1], x[2], wrap(x[3]), x[4], x[5],
        wrap(x[6]), wrap(x[7]), x[8], wrap(x[9]),
        x[10], x[11], x[12], x[13], sep = ", "
      )),
    ');'
  )

  sapply(inserts, function(x)
    dbGetQuery(db, x)) # push all the aliquots in...





  # update all the ones that changed

})


# some code for testing
SyncHandler$set("public", "demo", function() {

  library(BioradConfig)
  sqlite <- dbDriver("SQLite")
  dbname <- "inst/extdata/barcode.db"
  db_con <- dbConnect(sqlite, dbname)

  tok <- readRDS('~/.redcap/ecmo/token.rds')
  uri <- readRDS('~/.redcap/ecmo/uri.rds')
  gtok <- readRDS('~/.labguru/token.rds')

  s <- SyncHandler$new(project_id = 52, instrument = 'cytokine', redcap_id = 206,
                       redcap_token = tok, guru_token = gtok, redcap_url = uri)

  s$get_patient(db_con)

  s$get_redcap_data()
  s$get_local_data(db_con)

})

