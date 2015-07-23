

# gets a value for a certain data type and and id
get_one <- function(token, id, data_type) {
  target <- paste0("https://api.labguru.com/api/v1/", data_type, "/", id, 
                   ".json?token=", token)
  req <- curl::curl_fetch_memory(target)
  jsonlite::fromJSON(rawToChar(req$content))
}

# gets all of a certain data type via the api
# useful for tissues, boxes, storages etc
get_all <- function(token, data_type = 'boxes') {
  keep_going <- TRUE
  page_ctr <- 1
  all_storages <- list()
  while (keep_going) {
    target <- paste0("https://api.labguru.com/api/v1/", data_type, 
                     ".json?token=", token, "&page=", page_ctr)
    req <- curl::curl_fetch_memory(target)
    storages <- jsonlite::fromJSON(rawToChar(req$content))
    if (length(storages)) {
      all_storages[[page_ctr]] <- storages
      page_ctr <- page_ctr + 1
    } else {
      keep_going <- FALSE    
    }
    Sys.sleep(1)
  }
  return(all_storages)
}


# generic posting interface
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

# creates a box
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
  return(jsonlite::fromJSON(content(res, 'text')))
}


# creates a tissue
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
  return(jsonlite::fromJSON(content(res, 'text')))
}


# creates a tube given a ton of information about it....
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
      remarks = tube_notes,
      sample_uuid = tissue_uuid
    ),
    token = token
  )
  
  # send post request
  res <- poster(api_route = 'tubes', data = payload)
  return(jsonlite::fromJSON(content(res, 'text')))  
}

