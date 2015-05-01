#' Aliquot class
#'
#' @docType class
#' @export
#' @description A class representation of a cytokine aliquot.
#'
#' @format \code{\link{Aliquot}} class generator
#'
#' @usage \code{al = Aliquot$new()}
#'
#' @keywords data
#'
Aliquot <- R6::R6Class(
  "Aliquot",
  public = list(
    id = NA,
    barcode = NA,
    redcap_id = NA,
    plate_id = NA,
    is_depleted = NA,
    box_number = NA,
    row_letter = NA,
    col_number = NA,
    timepoint = NA
  )
)

