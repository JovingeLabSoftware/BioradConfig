#' Patient class
#'
#' @docType class
#' @export
#' @description A class representation of a patient.
#'
#' @format \code{\link{Patient}} class generator
#'
#' @usage \code{pat = Patient$new()}
#'
#' @keywords data
#'
Patient <- R6::R6Class(
  "Patient",
  public = list(
    id = NA,
    redcap_id = NA,
    project_id = NA,
    samples = list(),
    is_complete_0 = NA,
    is_complete_48 = NA,
    is_complete_192 = NA,
    all_complete = NA
  )
)

