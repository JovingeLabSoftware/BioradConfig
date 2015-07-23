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
  api_payload <- ECMOdev::create_api_config(api_url = uri, token = token)
  dd <- ECMOdev::get_data_dict(uri = api_payload$url, token = api_payload$tok)
  field_master <- c("record_id", grep("^bc", dd$field_name, value = TRUE))
  ecmo <- pull_barcodes(api_payload, field_master)
  ecmo_out <- cbind.data.frame(data.frame(pid = 52), ecmo)


  # pull control barcodes ---------------------------------------------------

  token <- readRDS("~/.redcap/controls/token.rds")
  api_payload <- ECMOdev::create_api_config(api_url = uri, token = token)
  dd <- ECMOdev::get_data_dict(uri = api_payload$url, token = api_payload$tok)
  field_master <- c("record_id", grep("^bc", dd$field_name, value = TRUE))
  controls <- pull_barcodes(api_payload, field_master)
  ctrl_out <- cbind.data.frame(data.frame(pid = 65), controls)
  names(ctrl_out) <- gsub("ctrl", "baseline", names(ctrl_out))


  # create global frame and return
  dat <- plyr::rbind.fill(ecmo_out, ctrl_out)
  return(dat)
}

