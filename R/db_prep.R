#!/usr/bin/env Rscript
# andrew borgman
# script to build our barcode database

library(RSQLite)
library(dplyr)
library(borgmisc)
library(readxl)
library(reshape2)




# bring in the barcode dump from redcap ----------------------------------------

# this lets you grab the latest version of the barcodes in REDCap
system('set-ip')
system('scp lab:/home/ubuntu/bc-dat/bcs.rds data/')

dat <- readRDS('data/bcs.rds')

dat[] <- lapply(dat, function(x) {
  x[x == ''] <- NA
  x
})

mm <- melt(dat, id.vars = c('pid', 'record_id'), factorsAsStrings = F)
mm <- mm[grep('^aliquot', mm$variable),]
mm$variable <- as.character(mm$variable)
tu <- setNames(c('0 Hour', '48 Hour', '8 Day'),
               c("aliquot_id", "aliquot_id48", "aliquot_id8"))
mm$variable <- tu[mm$variable]
mm <- mm[!is.na(mm$value),]


# parse the set of barcode locations in the freezer ----------------------------

# if we are connected to the share drive, copy the latest version
target <- '/Volumes/jovinge.lab/ECMO Study/BOX_ECMO-1_031715.xlsx'
if (file.exists(target)) {
  file.copy(target, 'data/BOX_ECMO-1_031715.xlsx', overwrite = TRUE)
}

# parse from the copy on share drive in the future...
boxes <- read_excel("data/BOX_ECMO-1_031715.xlsx", col_names = F)

aliquots <- list()

curr_box <- ''
curr_row <- ''
curr_col <- ''

# where are all of our boxes?
box_locs <- grep("^box", unlist(boxes[,1]), ignore.case = T)
sample_rows <- which(!is.na(unlist(boxes[,1])))
sample_rows <- setdiff(sample_rows, box_locs)


# add names for our selectors
box_locs <- setNames(box_locs,paste('Box', seq_along(box_locs)))
sample_rows <-
  setNames(sample_rows, trim(unlist(boxes[sample_rows, 1])))


# loop over our sample rows to populate our info
bc_ctr <- 1
bc_list <- list()
for (i in seq_along(sample_rows)) {
  curr_row <- sample_rows[i]
  curr_row_name <- names(sample_rows)[i]
  box_sel <- box_locs[box_locs < curr_row]
  curr_box <- names(box_sel)[which.max(box_sel)]

  # the actual barcodes are stored in the row above this...
  # and we *always* have 9 columns in a box, but they are offset by one...
  for (j in 2:10) {
    curr_col <- j - 1
    curr_bc <- trim(boxes[curr_row - 1, j])

    if (!is.na(curr_bc)) {
      bc_list[[bc_ctr]] <- data.frame(
        base_aliquot = strsplit(curr_bc, '-')[[1]][1],
        barcode = curr_bc,
        box = curr_box,
        row = curr_row_name,
        col = curr_col,
        stringsAsFactors = F
      )
      bc_ctr <- bc_ctr + 1
    }
  }
}

bc_locs <- do.call(rbind, bc_list)

# combine the two
dat <-
  merge(bc_locs, mm, by.x = 'base_aliquot', by.y = 'value', all = T)


# create our database -------------------------------------------

sqlite <- dbDriver("SQLite")
# dbname <- "data/plate-config.sqlite3"
dbname <- "data/production.db"

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
  "redcap_id" INTEGER,
  "is_depleted" INTEGER,
  "box_number" TEXT,
  "box_row" TEXT,
  "box_col" INTEGER,
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
pat_tab <- pat_tab[!is.na(pat_tab$record_id),]

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
    plate_col = 'null'
  ),
  dat[,c("box", "row", "col", "variable")]
)
ali_tab <- ali_tab[!is.na(ali_tab$barcode),]

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
    "box", "row", "col", "variable"
  )]

names(ali_tab) <-
  c(
    "plate_id" , "patient_id" , "barcode", "redcap_id" ,
    "is_depleted" , "box_number", "box_row", "box_col" ,
    "timepoint"
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
          sep = ", ")),
  ');'
)

sapply(inserts, function(x)
  dbGetQuery(db, x)) # push all the aliquots in...

# dbGetQuery(db, 'select * from aliquo

# disconnect from the database -------------------------------------------------
dbDisconnect(db)
