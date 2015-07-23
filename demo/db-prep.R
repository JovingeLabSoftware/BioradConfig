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
starts <- seq(3, (ncol(dat) - 3), 3)
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
stacked$timepoint <- tu[sapply(strsplit(stacked$bc_string, '_'), function(x) x[3])]
stacked <- stacked[!is.na(stacked$barcode), ]

stacked$base_aliquot <- sapply(strsplit(stacked$barcode, '-'), function(x) x[1])
dat <- stacked
dat$row <- substr(dat$location, 1, 1)
dat$col <- as.numeric(substr(dat$location, 2, 2))



# create our database -------------------------------------------

sqlite <- dbDriver("SQLite")
dbname <- "inst/extdata/barcode.db"
if (file.exists(dbname))
  file.remove(dbname)
db <- dbConnect(sqlite, dbname)

dbGetQuery(
  db,
  'CREATE TABLE plate
  ( "id" INTEGER PRIMARY KEY,
  "creation_date" TEXT,
  "run_date" TEXT,
  "is_processed" INTEGER,
  "layout" TEXT
  );'
)

dbGetQuery(
  db,
  'CREATE TABLE patient
  ( "id" INTEGER PRIMARY KEY,
  "redcap_id" INTEGER,
  "project_id" INTEGER,
  "is_complete_0" INTEGER,
  "is_complete_48" INTEGER,
  "is_complete_192" INTEGER,
  "all_complete" INTEGER
  );'
)

dbGetQuery(
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
  FOREIGN KEY(plate_id) REFERENCES plate(id),
  FOREIGN KEY(patient_id) REFERENCES patient(id)
  );'
)






# populating patient table -----------------------------------------------------

pat_tab <- cbind.data.frame(
  dat[,c("record_id", "pid")],
  data.frame(
    is_complete_0 = 0, is_complete_48 = 0, is_complete_192 = 0,
    all_complete = 0
  )
)
pat_tab <- pat_tab[!duplicated(pat_tab),]

names(pat_tab) <- c(
  "redcap_id", "project_id", "is_complete_0",
  "is_complete_48","is_complete_192", "all_complete"
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
    guru_tissue_id = 'null', guru_box_id = 'null'
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
    "guru_box_id"
  )]

###
# need to add these here
# "guru_tube_id" INTEGER,
# "guru_tissue_id" INTEGER,
# "guru_box_id" INTEGER,
###

names(ali_tab) <-
  c(
    "plate_id" , "patient_id" , "barcode", "redcap_id" ,
    "is_depleted" , "box_number", "box_row", "box_col" ,
    "timepoint", "guru_tube_id", "guru_tissue_id",
    "guru_box_id"
  )

# puke
wrap <- function(x)
  paste0('"', x, '"')

inserts <- paste0(
  'INSERT INTO aliquot (',
  paste(names(ali_tab), collapse = ", "),
  ') VALUES (',
  apply(ali_tab, 1, function(x)
    paste(x[1], x[2], wrap(x[3]), x[4], x[5],
          wrap(x[6]), wrap(x[7]), x[8], wrap(x[9]),
          x[10], x[11], x[12], sep = ", ")),
  ');'
)

sapply(inserts, function(x)
  dbGetQuery(db, x)) # push all the aliquots in...

# dbGetQuery(db, 'select * from aliquot')



# add in all our LabGuru information

pq <- "select * from patient where all_complete = 0;"
patients <- RSQLite::dbGetQuery(conn = db, statement = pq)
patients <- patients[sample(nrow(patients)), ]




# disconnect from the database -------------------------------------------------
dbDisconnect(db)







