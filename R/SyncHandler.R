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
      self$redcap_url <- redcap_token
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


# updates an aliquots plate assignment in the database
SyncHandler$set("public", "get_local_data", function(db_con) {

  # make sure db connection is right
  check_db(db_con)

  # which patient is this?
  q <- paste0("select id from patient where redcap_id = ", self$redcap_id,
              " and project_id = ", self$redcap_project)
  self$local_id <- unlist(dbGetQuery(db_con, q))

  # get all aliquots for that patient
  q <- paste0("select * from aliquot where patient_id = ", self$local_id)
  self$db_data <- dbGetQuery(db_con, q)

})


# some code for testing
SyncHandler$set("private", "testing", function() {
  sqlite <- dbDriver("SQLite")
  dbname <- "inst/extdata/barcode.db"
  db_con <- dbConnect(sqlite, dbname)

  self <- SyncHandler$new()


})






# updates an aliquots plate assignment in the database
SyncHandler$set("public", "sync_data", function(db_con) {

  check_db(db_con)

  # get our data from our data sources
  self$get_redcap_data()
  self$get_local_data(db_con)











})

