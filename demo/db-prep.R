#!/usr/bin/env Rscript
# andrew borgman
# script to build our barcode database

library(dplyr)
library(borgmisc)
library(reshape2)
library(BioradConfig)


# bring in the barcode dump from redcap ----------------------------------------


dat <- barcode_api_call()
dat[] <- lapply(dat, function(x) {
  x[x == ''] <- NA
  x
})

# stack this data frame -- kind of awkward
starts <- seq(3, ncol(dat), 3)

stacked <- do.call(rbind, lapply(starts, function(x) {
  cbind.data.frame(
    dat[,c(1, 2)],
    data.frame(bc_string = names(dat)[x], stringsAsFactors = FALSE),
    data.frame(
      barcode = dat[,x],
      box = dat[,x  + 1],
      location = dat[,x  + 2],
      stringsAsFactors = FALSE
    )
  )
}))


tu <- setNames(c('0 Hour', '48 Hour', '8 Day'),
               c("baseline", "48", "8"))
stacked$timepoint <-
  tu[sapply(strsplit(stacked$bc_string, '_'), function(x)
    x[3])]
stacked <- stacked[!is.na(stacked$barcode),]

stacked$base_aliquot <-
  sapply(strsplit(stacked$barcode, '-'), function(x)
    x[1])
dat <- stacked
dat$row <- substr(dat$location, 1, 1)
dat$col <- as.numeric(substr(dat$location, 2, 2))



# create our database -------------------------------------------

sqlite <- dbDriver("SQLite")
dbname <- "inst/extdata/barcode.db"
if (file.exists(dbname))
  file.remove(dbname)
db <- dbConnect(sqlite, dbname)

dbSendQuery(
  db,
  'CREATE TABLE plate
  ( "id" INTEGER PRIMARY KEY,
  "creation_date" TEXT,
  "run_date" TEXT,
  "is_processed" INTEGER,
  "layout" TEXT
  );'
)

dbSendQuery(
  db,
  'CREATE TABLE patient
  ( "id" INTEGER PRIMARY KEY,
  "redcap_id" INTEGER,
  "project_id" INTEGER,
  "is_complete_0" INTEGER,
  "tissue_0" INTEGER,
  "is_complete_48" INTEGER,
  "tissue_48" INTEGER,
  "is_complete_192" INTEGER,
  "tissue_192" INTEGER,
  "all_complete" INTEGER,
  FOREIGN KEY(tissue_0) REFERENCES tissue(id),
  FOREIGN KEY(tissue_48) REFERENCES tissue(id),
  FOREIGN KEY(tissue_192) REFERENCES tissue(id)
  );'
)

dbSendQuery(
  db,
  'CREATE TABLE aliquot
  ( "id" INTEGER PRIMARY KEY,
  "plate_id" INTEGER,
  "plate_row" TEXT,
  "plate_col" INTEGER,
  "patient_id" INTEGER,
  "barcode" TEXT,
  "guru_tube_id" INTEGER,
  "redcap_id" INTEGER,
  "guru_tissue_id" INTEGER,
  "is_depleted" INTEGER,
  "box_number" TEXT,
  "box_row" TEXT,
  "box_col" INTEGER,
  "guru_box_id" INTEGER,
  "timepoint" TEXT,
  "in_guru" INTEGER,
  FOREIGN KEY(plate_id) REFERENCES plate(id),
  FOREIGN KEY(patient_id) REFERENCES patient(id)
  );'
)

# also create tables for LabGuru tracking so we don't have to beat the API to
# death during our sync calls
dbSendQuery(
  db,
  'CREATE TABLE box
  ( "id" INTEGER PRIMARY KEY,
  "labguru_id" INTEGER,
  "labguru_name" TEXT
  );'
)

dbSendQuery(
  db,
  'CREATE TABLE tissue
  ( "id" INTEGER PRIMARY KEY,
  "labguru_id" INTEGER,
  "labguru_name" TEXT,
  "labguru_uuid" TEXT
  );'
)




# populating patient table -----------------------------------------------------

pat_tab <- cbind.data.frame(
  dat[,c("record_id", "pid")],
  data.frame(
    is_complete_0 = 0, tissue_0 = 'null', is_complete_48 = 0, tissue_48 = 'null',
    is_complete_192 = 0, tissue_192 = 'null', all_complete = 0
  )
)
pat_tab <- pat_tab[!duplicated(pat_tab),]

names(pat_tab) <- c(
  "redcap_id", "project_id", "is_complete_0", "tissue_0", "is_complete_48",
  "tissue_48", "is_complete_192", "tissue_192", "all_complete"
)

inserts <- paste0(
  'INSERT INTO patient (',
  paste(names(pat_tab), collapse = ", "),
  ') VALUES (',
  Reduce(function(...)
    paste(..., sep = ", "), pat_tab),
  ');'
)

sapply(inserts, function(x)
  dbGetQuery(db, x)) # push all the records in...




# creating aliquot table -------------------------------------------------------

ali_tab <- cbind.data.frame(
  dat[,c("barcode", "record_id", "pid")],
  data.frame(
    is_depleted = 0, plate_id = 'null', plate_row = 'null',
    plate_col = 'null', guru_tube_id = 'null',
    guru_tissue_id = 'null', guru_box_id = 'null',
    in_guru = 'null'
  ),
  dat[,c("box", "row", "col", "timepoint")]
)

ali_tab$key <- paste(ali_tab$record_id, ali_tab$pid, sep = '_')


# bring in patient table to match up keys
pats <- dbGetQuery(db, "select * from patient")
pats$key <- paste(pats$redcap_id, pats$project_id, sep = '_')
pats <- pats[,c('id', 'key')]
ali_tab <- merge(ali_tab, pats, by = 'key')
ali_tab <- ali_tab[,-which(names(ali_tab) == 'key')]

ali_tab <-
  ali_tab[,c(
    "plate_id", "id", "barcode", "record_id", "is_depleted",
    "box", "row", "col", "timepoint", "guru_tube_id", "guru_tissue_id",
    "guru_box_id", "in_guru"
  )]

names(ali_tab) <-
  c(
    "plate_id" , "patient_id" , "barcode", "redcap_id" ,
    "is_depleted" , "box_number", "box_row", "box_col" ,
    "timepoint", "guru_tube_id", "guru_tissue_id",
    "guru_box_id", "in_guru"
  )

# puke
wrap <- function(x)
  paste0('"', x, '"')

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

# add in all our LabGuru information -------------------------------------------

gtok <- readRDS('~/.labguru/token.rds')
gbox <- get_all(token = gtok, data_type = 'boxes')
gbox <- do.call(rbind, lapply(gbox, function(x) x[,c('id', 'name')]))
gtis <- get_all(token = gtok, data_type = 'tissues')
gtis <-
  do.call(rbind, lapply(gtis, function(x)
    x[,c('id', 'name', 'uuid')]))
gtub <- get_all(token = gtok, data_type = 'tubes')
gtub <-
  do.call(rbind, lapply(gtub, function(x)
    cbind(
      x[,c('id', 'name', 'barcode')],
      data.frame(
        box_name = x$box$name,
        box_loc = x$box$location_in_box
      )
    )))


# there is an error on one off the calls for a "page" of aliquots
# figure out which ones these are and grab them one by one
maxx <- max(11501 + 1000, max(gtub$id))
id_range <- seq(1, maxx, 10)



missed <- id_range[which(!(id_range %in% gtub$id))]
miss_tubes <- lapply(missed, function(x) {
  tryCatch({
    get_one(token = gtok, id = x, data_type = 'tubes')
  },
  error = function(e) NULL)
})
miss_tubes <- miss_tubes[sapply(miss_tubes, function(x) !is.null(x))]
miss_tubes <- do.call(rbind, lapply(miss_tubes, function(x) {
  data.frame(
    id = x$id,
    name = x$name,
    barcode = ifelse(is.null(x$barcode), NA, x$barcode),
    box_name = x$box$name,
    box_loc = x$box$location_in_box
  )
}))

gtub <- rbind(gtub, miss_tubes)


# get all the aliquots in our database to update
alis <- RSQLite::dbGetQuery(conn = db, statement =  "select * from aliquot;")
ali_list <- lapply(1:nrow(alis), function(x) {
  Aliquot$new(alis[x,])
})


tlu <- setNames(c('cytokine', 'pbmc', 'neutrophil'), c('CT', 'PB', 'NT'))


for (i in seq_along(ali_list)) {

  ali <- ali_list[[i]]
  bsel <- which(gbox[["name"]] == ali$box_number)
  tsel <- which(gtis$name == strsplit(ali$barcode, '-')[[1]][1])
  asel <- which(gtub$name == ali$barcode)

  if (length(asel)) {

    tmp <- gtub[asel,]
    bmatch <- intersect(
      which(tmp$box_name == ali$box_number),
      which(tmp$box_loc == alpha_to_guru(ali$get_loc()))
      )

    if (length(bmatch) == 1) {
      ali$update_value_in_db(db_con = db, column_name = 'guru_tube_id',
                             value = tmp$id[bmatch])
      ali$update_value_in_db(db_con = db, column_name = 'guru_tissue_id',
                             value = gtis$id[tsel])
      ali$update_value_in_db(db_con = db, column_name = 'guru_box_id',
                             value = gbox$id[bsel])
      ali$update_value_in_db(db_con = db, column_name = 'in_guru', value = 1)

    } else {
      print(paste('Could not find unique match for', ali$barcode))
      print(paste('i =', i))
    }

  } else {

    # create the tube (and possible tissue) in LabGuru
    if (!length(tsel)) {
      base_id <- strsplit(ali$barcode, '-')[[1]][1]
      tissue_descr <- paste(
        paste("Patient Type:", ifelse(
          grepl("CTRL", ali$barcode), 'control',
          'patient'
        )),
        paste("Collection Timepoint:", ali$timepoint),
        sep = "<br/>"
      )

      tissue_info <- create_tissue(base_id = base_id, token = gtok,
                                   descr = tissue_descr)
      gtis <- rbind(gtis, data.frame(tissue_info[c('id', 'name', 'uuid')]))
      tiss_uuid <- tissue_info[['uuid']]
      tiss_id <- tissue_info[['id']]
    } else {
      tiss_uuid <- gtis$uuid[tsel]
      tiss_id <- gtis$id[tsel]
    }

    tstr <- strsplit(ali$barcode, '-')[[1]][2]
    stype <- ifelse(is.na(tstr), 'unknown', tlu[tstr])
    tube_descr <- paste(
      paste("Sample Type:", stype),
      paste("Patient Type:", ifelse(
        grepl("CTRL", ali$barcode), 'control',
        'patient'
      )),
      paste("Collection Timepoint:", ali$timepoint),
      sep = "<br/>"
    )

    tube_info <- create_tube(
      tube_name = ali$barcode,
      tube_barcode = ali$barcode,
      box = gbox$id[bsel],
      box_location = alpha_to_guru(ali$get_loc()),
      tissue_uuid = tiss_uuid,
      tube_notes = tube_descr,
      token = gtok
    )

    ali$update_value_in_db(db_con = db, column_name = 'guru_tube_id',
                           value = tube_info[['id']])
    ali$update_value_in_db(db_con = db, column_name = 'guru_tissue_id',
                           value = tiss_uuid)
    ali$update_value_in_db(db_con = db, column_name = 'guru_box_id',
                           value = gbox$id[bsel])
    ali$update_value_in_db(db_con = db, column_name = 'in_guru', value = 1)

  }
}



# write out labguru information into the database ------------------------------

#
names(gbox) <- c("labguru_id", "labguru_name")


inserts <- paste0(
  'INSERT INTO box (',
  paste(names(gbox), collapse = ", "),
  ') VALUES (',
  apply(gbox, 1, function(x)
    paste(x[1], wrap(x[2]), sep = ", ")),
  ');'
)

sapply(inserts, function(x)
  dbGetQuery(db, x)) # push all the records in...


names(gtis) <- c("labguru_id", "labguru_name", "labguru_uuid")
inserts <- paste0(
  'INSERT INTO tissue (',
  paste(names(gtis), collapse = ", "),
  ') VALUES (',
  apply(gtis, 1, function(x)
    paste(x[1], wrap(x[2]), wrap(x[3]), sep = ", ")),
  ');'
)

sapply(inserts, function(x)
  dbGetQuery(db, x)) # push all the records in...



# disconnect from the database -------------------------------------------------
dbDisconnect(db)
