
#' Create API configuration
#'
#' Function is used to create a named list of API configuration to be
#' passed to any of the methods that interact with the
#' REDCap API. Current defaults include using SSL certificates from
#' \code{REDCapR} package.
#'
#' @param api_url The API URL of the REDCap instance you'd like to connect to.
#' @param token The API token for the project you'd like to connect to.
#' @param config_options A list of \code{CURL} options to be passed to
#'  \code{httr::POST}. See ?httr::config for full details.
#'
#' @export

create_api_config <- function(api_url = NULL, token = NULL,
                              config_options = httr::config(
                                cainfo = system.file(
                                  "ssl_certs/mozilla_ca_root.crt",
                                  package = "REDCapR"
                                ),
                                timeout = 8,
                                ssl_verifypeer = 1)) {

  if (is.null(api_url)) stop('You must provide a url to the API...')
  if (is.null(token)) stop('You must provide a token for the API...')
  payload <- list(tok = token, url = api_url, config_options = config_options)
  return(payload)
}


#' Get data dictionary as \code{data.frame}
#'
#' \code{get_data_dict} returns a \code{data.frame} containing the project's Data Dictionary
#'
#'
#' @param api_config A list resulting from \code{create_api_config}
#'

get_data_dict <- function(api_config) {

  post_body <- list(token = api_config$tok, content = 'metadata',
                    format = 'json')
  result <- httr::POST(url = api_config$url, body = post_body,
                       config = api_config$config_options)

  if (result$status_code != 200L)
    stop('Could not connect to API to get data dictionary...')

  raw_text <- httr::content(result, "text")
  data_dict <- jsonlite::fromJSON(raw_text)
  return(invisible(data_dict))
}


#' Get some values out of REDCap
#'
#' Returns a data frame with values for the specified field names for the
#' specified patient
#'
#' @param api_config A list resulting from \code{create_api_config}
#' @param fnames A vector of field names in REDCap
#' @param record_id A REDCap ID for a record in the project
#'

get_redcap_values <- function(api_payload, fnames, record_id) {

  # try this up to 10 times before giving up...
  n <- 10
  trys <- 0
  worked <- FALSE

  # pull barcode info via api
  post_body <- list(
    token = api_payload$tok, content = 'record', format = 'json', type = 'flat',
    fields = paste(fnames, collapse = ","), rawOrLabel = 'label',
    records = record_id
  )

  while (trys < n & !worked) {
    trys <- trys + 1
    print(sprintf("Tried to contact REDCap %i times...", trys))
    try({
      result <- httr::POST(
        url = api_payload$url, body = post_body, config = api_payload$config_options
      )
      worked <- TRUE
    })
  }

  if ((result$status_code != 200L) | !worked) stop('Could not contact REDCap...')

  raw_text <- httr::content(result, "text")
  bcs <- jsonlite::fromJSON(raw_text)

  # toss our our samples without any bardocs
  if (ncol(bcs) > 2) {
    sel <- !apply(bcs[,2:ncol(bcs)], 1, function(x) all(x == ''))
  } else {
    sel <- !(bcs[,2] == '')
  }
  return(bcs[sel, ])
}






#' Pull barcode inventory from REDCap API
#'
#' This function will grab the latest inventory information from REDCap and
#' return it as a data frame. Currently supports ECMO and control projects;
#' everything is hard coded and we may want to update in the future. However,
#' once this is in production, there should be no need to run this script.
#'
#' @export
#' @examples
#' barcode_api_call()

barcode_api_call <- function() {

  # helper functions ---------------------------------------------------------

  pull_barcodes <- function(api_payload, fnames) {

    # try this up to 10 times before giving up...
    n <- 10
    trys <- 0
    worked <- FALSE

    # pull barcode info via api
    post_body <- list(
      token = api_payload$tok, content = 'record', format = 'json', type = 'flat',
      fields = paste(fnames, collapse = ","), rawOrLabel = 'label'
    )

    while (trys < n & !worked) {
      trys <- trys + 1
      print(sprintf("Tried to contact REDCap %i times...", trys))
      try({
        result <- httr::POST(
          url = api_payload$url, body = post_body, config = api_payload$config_options
        )
        worked <- TRUE
      })
    }

    if ((result$status_code != 200L) | !worked) stop('Could not contact REDCap...')

    raw_text <- httr::content(result, "text")
    bcs <- jsonlite::fromJSON(raw_text)

    # toss our our samples without any bardocs
    if (ncol(bcs) > 2) {
      sel <- !apply(bcs[,2:ncol(bcs)], 1, function(x) all(x == ''))
    } else {
      sel <- !(bcs[,2] == '')
    }
    return(bcs[sel, ])
  }



  # global api info ----------------------------------------------------------
  uri <- readRDS("~/.redcap/ecmo/uri.rds")


  # pull ecmo barcodes ------------------------------------------------------

  token <- readRDS("~/.redcap/ecmo/token.rds")
  api_payload <- create_api_config(api_url = uri, token = token)
  dd <- get_data_dict(api_payload)
  field_master <- c("record_id", grep("^bc", dd$field_name, value = TRUE))
  ecmo <- pull_barcodes(api_payload, field_master)
  ecmo_out <- cbind.data.frame(data.frame(pid = 52), ecmo)


  # pull control barcodes ---------------------------------------------------

  token <- readRDS("~/.redcap/controls/token.rds")
  api_payload <- create_api_config(api_url = uri, token = token)
  dd <- get_data_dict(api_payload)
  field_master <- c("record_id", grep("^bc", dd$field_name, value = TRUE))
  controls <- pull_barcodes(api_payload, field_master)
  ctrl_out <- cbind.data.frame(data.frame(pid = 65), controls)
  names(ctrl_out) <- gsub("ctrl", "baseline", names(ctrl_out))


  # create global frame and return
  dat <- plyr::rbind.fill(ecmo_out, ctrl_out)
  return(dat)
}

