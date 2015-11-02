#' Get one of something from LabGuru
#'
#' This function allows you to pull something (tube, tissue, etc) from
#' the LabGuru API.
#'
#' @param token A valid LabGuru API token
#' @param id The ID of the object you would like to retrieve
#' @param data_type What kind of object \code{tubes}, \code{tissues}, or \code{boxes}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get a box via the api
#' tok <- 'API_SECRET'
#' box_info <- get_one(token = tok, id = 70, data_type = 'boxes')
#'
#' }

# gets a value for a certain data type and and id
get_one <- function(token, id, data_type) {
  target <- paste0("https://api.labguru.com/api/v1/", data_type, "/", id,
                   ".json?token=", token)
  req <- curl::curl_fetch_memory(target)
  jsonlite::fromJSON(rawToChar(req$content))
}

#' Get all of something from LabGuru
#'
#' This function allows you to pull all of something (tube, tissue,
#' etc) from the LabGuru API.
#'
#' @param token A valid LabGuru API token
#' @param data_type What kind of object tube, tissue, etc
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get a box via the api
#' tok <- 'API_SECRET'
#' box_info <- get_all(token = tok, data_type = 'boxes')
#'
#' }

get_all <- function(token, data_type = 'boxes') {
  keep_going <- TRUE
  page_ctr <- 1
  all_storages <- list()


  # reqs <- list()

  while (keep_going) {
    target <- paste0("https://api.labguru.com/api/v1/", data_type,
                     ".json?token=", token, "&page=", page_ctr)
    req <- curl::curl_fetch_memory(target)
    # reqs[[page_ctr]] <- req


    e <- try({storages <- jsonlite::fromJSON(rawToChar(req$content))},
             silent = TRUE)
    if (inherits(e, 'try-error')) {
      all_storages[[page_ctr]] <- NULL
      page_ctr <- page_ctr + 1
    }
    if (length(storages)) {
      all_storages[[page_ctr]] <- storages
      page_ctr <- page_ctr + 1
    } else {
      keep_going <- FALSE
    }
    Sys.sleep(0.5)
  }
  return(all_storages)
}


# generic POST interface
poster <- function(api_route, data) {
  req <- httr::POST(
    url = paste0("https://api.labguru.com/api/v1/", api_route, ".json"),
    config = httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(data, auto_unbox = TRUE)
  )
  if (req$status_code != 201L) {
    stop(paste0('There was an error creating ', data[['name']]))
  } else {
    return(req)
  }
}




#' Create a box in LabGuru
#'
#' This function lets you create a box via the LabGuru API.
#'
#' @param box_name The name of the box to create
#' @param token A valid LabGuru API token
#' @param box_rows Number of rows in the box
#' @param box_cols Number of columns in the box
#' @param data_type What kind of object tube, tissue, etc
#'
#' @export

create_box <- function(box_name, token, box_rows = 9, box_cols = 9,
                       type = 'Tissue', color ='gray') {
  payload <- list(
    item = list(
      name = box_name,
      rows = box_rows,
      cols = box_cols,
      box_type = type,
      color = color,
      member_id = 2000011
    ),
    token = token
  )

  # send post request
  res <- poster(api_route = 'boxes', data = payload)
  return(jsonlite::fromJSON(httr::content(res, 'text')))
}


#' Create a tissue in LabGuru
#'
#' This function lets you create a box via the LabGuru API.
#'
#'
#' @export

create_tissue <- function(base_id, descr, token, type = "blood") {
  payload <- list(
    item = list(
      name = base_id,
      description = descr,
      tissue_type = type
    ),
    token = token
  )

  # send post request
  res <- poster(api_route = 'tissues', data = payload)
  return(jsonlite::fromJSON(httr::content(res, 'text')))
}


#' Create a tube in LabGuru
#'
#' This function lets you create a tube via the LabGuru API.
#'
#'
#' @export

create_tube <- function(tube_name, tube_barcode, box, box_location, tissue_uuid,
                        tube_notes, token) {

  # build up our post body
  payload <- list(
    item = list(
      member_id = 2000011, # upload as myself for now...
      name = tube_name,
      barcode = tube_barcode,
      box_id = box,
      location_in_box = as.character(box_location), # this has to be a character?
      description = tube_notes,
      sample_uuid = tissue_uuid
    ),
    token = token
  )

  # send post request
  res <- poster(api_route = 'stocks', data = payload)
  return(jsonlite::fromJSON(httr::content(res, 'text')))
}




#' Compute the LabGuru box position
#'
#' This function takes an alphanumeric box location (ex. B2) and computes the
#' corresponding numerical position used in LabGuru. Passing
#' \code{reverse = TRUE} allows users to perform the computation in revers --
#' i.e. to compute the alphanumeric position from the LabGuru position.
#'
#' @param loc_string The value to convert
#' @param reverse Which direction do you want to go
#' @param box_cols How many columns are in the storage box
#'
#' @export


alpha_to_guru <- function(loc_string, reverse = FALSE, box_cols = 9) {
  if (!reverse) {
    if (nchar(loc_string) != 2 | is.numeric(loc_string))
      stop('Invalid loc_string...')
    roe <- toupper(substr(loc_string, 1, 1))
    column <- as.numeric(substr(loc_string, 2, 2))
    row_mult <- match(roe, LETTERS) - 1
    box_loc <- (row_mult * box_cols) + column
    return(box_loc)
  } else {
    if (!is.numeric(loc_string))
      stop('loc_string must be numeric if reverse = TRUE')
    roe <- LETTERS[ceiling(loc_string / box_cols)]
    column <- loc_string %% box_cols
    return(paste0(roe, column))
  }
}


#' Get a new API key
#'
#' This function will get an updated API token from LabGuru to replace an
#' expired one.
#'
#' @param user_name Your LabGuru user name
#' @param password Your LabGuru password
#'
#' @examples
#' \dontrun{
#' # get a new token
#' my_secrets = readRDS('~/.labguru.rds')
#' new_api_key <- get_guru_key(my_secrets[['user']], my_secrets[['pass']])
#' }
#'
#' @export

get_guru_key <- function(user_name, password) {

  res <- httr::POST(
    url = "https://api.labguru.com/api/v1/sessions.json",
    config = httr::add_headers("Content-Type" = "application/json"),
    body = paste0('{"login":"', user_name, '","password":"',  password, '"}')
  )

  if (res$status_code != 200L) {
    stop('There was an error retrieving your API token....')
  } else {
    return(httr::content(res)$token)
  }

}


#' Check if an existing API key works
#'
#' This function will test an API token from LabGuru - returning \code{TRUE} if
#' the token is valid and \code{FALSE} otherwise.
#'
#' @param token The API token to test
#'
#' @export

check_guru_key <- function(token) {
  # do something simple and see if it fails
  e <- try({get_one(token = token, id = 1, data_type = 'stocks')})
  if (inherits(e, 'try-error')) return(FALSE)
  else return(TRUE)
}




# generic PUT interface
putter <- function(api_route, id, data) {
  req <- httr::PUT(
    url = paste0("https://api.labguru.com/api/v1/", api_route, "/", id, ".json"),
    config = httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(data, auto_unbox = TRUE)
  )
  if (req$status_code != 200L) {
    stop(paste0('There was an error creating ', data[['name']]))
  } else {
    return(req)
  }
}


#' Update a tissue in LabGuru
#'
#' This function lets you update an existing tissue via the LabGuru API.
#'
#' @param id The numeric ID of the tissue
#' @param name The character name of the tissue
#' @param token A valid API token
#' @param description An updated description of the tissue
#' @param tissue_type An updated description of the tissue
#'
#' @export

update_tissue <- function(id, name, token, description = NULL,
                          tissue_type = NULL) {

  if (missing(id)) stop('You must provide labguru_id')
  if (missing(name)) stop('You must provide labguru_name')
  if (missing(token)) stop('You must provide token')

  # only pass non-null args
  item_list <- as.list(environment())
  sel <- !(sapply(item_list, is.null) | names(item_list) == 'token')
  item_list <- item_list[sel]


  payload <- list(
    item = item_list,
    token = token
  )

  # send put request
  res <- putter(api_route = 'tissues', id = id, data = payload)
  return(jsonlite::fromJSON(httr::content(res, 'text')))
}


#' Update a tube in LabGuru
#'
#' This function lets you update an existing tube via the LabGuru API. Values
#' set as \code{NULL} will not be updated.
#'
#' @param id The numeric ID of the tube
#' @param token A valid API token
#' @param name The character name of the tube
#' @param remarks An updated description for the tube
#' @param barcode An updated barcode for the tube
#' @param box_id An updated holding box for the tube (numeric LabGuru ID)
#' @param location_in_box An updated box location for the tube
#' @param sample_uuid An updated tissue to associate with the tube
#' @param member_id An updated owner for the tube
#'
#' @export

update_tube <- function(id, token, name = NULL, remarks = NULL, barcode = NULL,
                        box_id = NULL, location_in_box = NULL,
                        sample_uuid = NULL, member_id = NULL) {

  if (missing(id)) stop('You must provide id')
  if (missing(token)) stop('You must provide token')

  # only pass non-null args
  item_list <- as.list(environment())
  sel <- !(sapply(item_list, is.null) | names(item_list) == 'token')
  item_list <- item_list[sel]
  payload <- list(
    item = item_list,
    token = token
  )

  # send put request
  res <- putter(api_route = 'tubes', id = id, data = payload)
  return(jsonlite::fromJSON(httr::content(res, 'text')))
}


get_guru_locstring <- function(bc, tp) {
  tlu <- setNames(c('cytokine', 'pbmc', 'neutrophil'), c('CT', 'PB', 'NT'))
  tstr <- strsplit(bc, '-')[[1]][2]
  stype <- ifelse(is.na(tstr), 'unknown', tlu[tstr])
  tube_descr <- paste(
    paste("Sample Type:", stype),
    paste("Patient Type:", ifelse(
      grepl("CTRL", bc), 'control',
      'patient'
    )),
    paste("Collection Timepoint:", tp),
    sep = "<br/>"
  )
  return(tube_descr)
}
