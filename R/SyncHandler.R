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
#' @usage \code{
#' sync <- SyncHandler$new(project_id, instrument, redcap_id, redcap_token,
#'                        guru_token, redcap_url)
#' sync$sync_data()
#' }
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
    to_create = NA,
    to_update = NA,


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

  if (nrow(barcodes)) {
    # stack this data frame -- kind of awkward
    starts <- seq(2, ncol(barcodes), 3)
    self$redcap_data <- do.call(rbind, lapply(starts, function(x) {
      cbind.data.frame(
        data.frame(bc_string = names(barcodes)[x], stringsAsFactors = FALSE),
        data.frame(
          barcode = barcodes[,x],
          box = barcodes[,x + 1],
          location = barcodes[,x + 2],
          sample_type = strsplit(names(barcodes)[x], '_')[[1]][2],
          stringsAsFactors = FALSE
        )
      )
    }))
  } else {
    self$redcap_data <- NULL # make sure this is null even though it should be
  }

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
    message('Creating new object for REDCap patient ', self$redcap_id)
    pat <- Patient$new()
    pat$redcap_id <- self$redcap_id
    pat$project_id <- self$redcap_project
    pat$is_complete_0 <- pat$is_complete_48 <- pat$is_complete_192 <- pat$all_complete <- 0
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

      message('Creating tissue: ', tmp$base_bc[miss_sel[i]])

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
      message('Creating box: ', all_boxes[miss_sel[i]])
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



# figures out what needs to be updated and what needs to be added to LabGuru
# by diffing our data sets; writes results to self$to_create = NA and
# self$to_update
SyncHandler$set("public", "find_changes", function() {

  # run through our values and see what is different or missing
  to_update <- c()
  to_add <- c()

  for (i in 1:nrow(self$redcap_data)) {
    if (self$redcap_data$barcode[i] != "") {

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
          message('This row matches multiple entries in our current database:')
          message(self$redcap_data[i, ])
          message('\n')
          stop('This should not be possible...')

        }
      }
    }
  }

  if (length(to_add))
    self$to_create <- self$redcap_data[to_add, ]

  if (length(to_update))
    self$to_update <- self$redcap_data[to_update, ]

})


# creates a set of new aliquots in the database and in LabGuru
SyncHandler$set("public", "create_new_aliquots", function(db_con) {

  if (!is.data.frame(self$to_create)) {
    message('No aliquots to create...')
    return()
  } else {
    for (i in 1:nrow(self$to_create)) {

      # pull some data from the database
      tissue_info <- dbGetQuery(
        conn = db_con,
        statement = paste0(
          'select * from tissue where labguru_name = ',
          wrap(get_base_bc(self$to_create$barcode[i])),
          ';'
        )
      )

      box_info <- dbGetQuery(
        conn = db_con,
        statement = paste0(
          'select * from box where labguru_name = ',
          wrap(self$to_create$box[i]),
          ';'
        )
      )

      # set the fields that we can
      new_ali <- data.frame(matrix(NA, ncol = ncol(self$db_data), nrow = 1))
      names(new_ali) <- names(self$db_data)
      tk <- which(names(new_ali) %in% c("id", "plate_id", "plate_row", "plate_col"))
      new_ali <- new_ali[,-tk]

      new_ali$barcode <- self$to_create$barcode[i]
      new_ali$timepoint <- get_tp(self$to_create$bc_string[i])
      new_ali$redcap_id <- self$redcap_id
      new_ali$patient_id <- self$local_id
      new_ali$is_depleted <- 0
      new_ali$box_number <- self$to_create$box[i]
      new_ali$box_row <- substr(self$to_create$location[i], 1, 1)
      new_ali$box_col <- substr(self$to_create$location[i], 2, 2)
      new_ali$guru_box_id <- box_info$labguru_id
      new_ali$guru_tissue_id <- tissue_info$labguru_id
      new_ali$sample_type <- self$to_create$sample_type[i]


      message('Creating tube for ', new_ali$barcode, ' in LabGuru...')

      tube_info <- create_tube(
        tube_name = new_ali$barcode,
        tube_barcode = new_ali$barcode,
        box = new_ali$guru_box_id,
        box_location = alpha_to_guru(self$to_create$location[i]),
        tissue_uuid = tissue_info$labguru_uuid,
        tube_notes = get_guru_locstring(new_ali$barcode, new_ali$timepoint),
        token = self$guru_token
      )

      new_ali$guru_tube_id <- tube_info[['id']]
      new_ali$in_guru <- 1
      x <- unlist(new_ali[1,])

      message('Creating aliquot ', new_ali$barcode, ' in database...')

      ins <- paste0(
        'INSERT INTO aliquot (',
        paste(names(new_ali), collapse = ", "),
        ') VALUES (',
        paste(
          x[1], wrap(x[2]), x[3], x[4], x[5],
          x[6], wrap(x[7]), wrap(x[8]), x[9],
          x[10], wrap(x[11]), x[12], wrap(x[13]), sep = ", "
        ),
        ');'
      )
      dbSendQuery(db_con, ins)

    }
  }

})


# updates storage box and location of any aliqouts if they were changed in REDCap
SyncHandler$set("public", "update_aliquots", function(db_con) {

  if (!is.data.frame(self$to_update)) {
    message('No aliquots to update...')
    return()
  } else {
    for (i in 1:nrow(self$to_update)) {

      # grab some info from the database and create and aliquot
      r <- dbGetQuery(conn = db_con,
                      statement =
                        paste0(
                          'select * from aliquot where barcode = ',
                          wrap(self$to_update$barcode[i])
                        ))

      # create our object and update its fields
      al <- Aliquot$new(r)
      al$box_number <- self$to_update$box[i]
      al$box_row <- substr(self$to_update$location[i], 1, 1)
      al$box_col <- as.numeric(substr(self$to_update$location[i], 2, 2))

      if (self$local_id != al$patient_id) {
        stop('You somehow managed to match a barcode but not a patient...?')
      }

      # update in our database
      message('Updating location of ', self$to_update$barcode[i], " in database...")
      al$update_value_in_db(db_con = db_con, column_name = 'box_number',
                            value = al$box_number)
      al$update_value_in_db(db_con = db_con, column_name = 'box_row',
                            value = al$box_row)
      al$update_value_in_db(db_con = db_con, column_name = 'box_col',
                            value = al$box_col)

      message('Updating location of ', self$to_update$barcode[i], " in LabGuru...")
      new_box <-
        unlist(dbGetQuery(
          conn = db_con, statement = paste0(
            "select labguru_id from box where labguru_name = ", wrap(al$box_number)
          )
        ))

      update_tube(
        id = al$guru_tube_id, token = self$guru_token,
        box_id = new_box,
        location_in_box = alpha_to_guru(al$get_loc())
      )
    }
  }
})



# updates an aliquots plate assignment in the database
SyncHandler$set("public", "sync_data", function(db_con) {

  check_db(db_con)

  message('Beginning sync for patient ', self$redcap_id, ' from project ',
          self$redcap_project, ' at ', Sys.time(), '...')

  # make sure our patient exists and set our local_id field telling us what the
  # ID of the patient in the database is
  self$get_patient(db_con)

  # get all the data we need from REDCap -- used for setting tissues
  self$get_redcap_data()

  # only perform the syncing if the data already exists
  if (!is.null(self$redcap_data)) {

    # make sure all the tissues exist
    self$check_tissues(db_con)

    # make sure all the boxes exist
    self$check_boxes(db_con)

    # grab local data to compare with what is in REDCap
    self$get_local_data(db_con)

    # figure out which aliquots need to be created and updated
    self$find_changes()

    # add all the new aliquots
    self$create_new_aliquots(db_con)

    # update those that need to be updated
    self$update_aliquots(db_con)

  } else {
    message("No barcodes in REDCap to sync. Exiting...")
  }

  message('Sync for patient ', self$redcap_id, ' from project ',
          self$redcap_project, ' finished at ', Sys.time(), '...')

  message('----------------------\n')


})


# some code for testing
SyncHandler$set("public", "demo", function() {

  library(BioradConfig)
  sqlite <- dbDriver("SQLite")
  dbname <- "~/dbs/barcode.db"
  db_con <- dbConnect(sqlite, dbname)

  tok <- readRDS('~/.redcap/ecmo/token.rds')
  uri <- readRDS('~/.redcap/ecmo/uri.rds')
  gtok <- readRDS('~/.labguru/token.rds')

  # let's test this out -- creating dummy barcodes for one of our retrospective patients
  # don't forget to delete these later....
  # editing ID 2 in project 52
  # BC00000
  # BC00000-CT-01
  # BC00000-CT-02
  # BC00000-CT-03
  # BC00000-CT-04
  # FakeBox
  # BC00000-NT-01
  # BC00000-NT-02
  # BC00000-NT-03
  # BC00000-NT-04
  # FakeBox2

  s <- SyncHandler$new(project_id = 65, instrument = 'cytokine', redcap_id = 65,
                       redcap_token = tok, guru_token = gtok, redcap_url = uri)

  s$sync_data(db_con = db_con)


  s <- SyncHandler$new(project_id = 52, instrument = 'cytokine', redcap_id = 209,
                       redcap_token = tok, guru_token = gtok, redcap_url = uri)

  s$sync_data(db_con = db_con)

})

