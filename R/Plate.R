#' Plate class
#'
#' @docType class
#' @export
#' @description A class representation of a Bio-Rad plate.
#'
#' @format \code{\link{Plate}} class generator
#'
#' @usage \code{plate = Plate$new()}
#'
#' @keywords data
#'
Plate <- R6::R6Class(
  "Plate",
  public = list(
    id = NA,
    samples = list(),
    run_date = NA,
    is_processed = NA,
    results = list()
  )
)

