% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap-utils.R
\name{barcode_api_call}
\alias{barcode_api_call}
\title{Pull barcode inventory from REDCap API}
\usage{
barcode_api_call()
}
\description{
This function will grab the latest inventory information from REDCap and
return it as a data frame. Currently supports ECMO and control projects;
everything is hard coded and we may want to update in the future. However,
once this is in production, there should be no need to run this script.
}
\examples{
barcode_api_call()
}

