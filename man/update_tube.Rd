% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guru-utils.R
\name{update_tube}
\alias{update_tube}
\title{Update a tube in LabGuru}
\usage{
update_tube(id, token, name = NULL, remarks = NULL, barcode = NULL,
  box_id = NULL, location_in_box = NULL, sample_uuid = NULL,
  member_id = NULL)
}
\arguments{
\item{id}{The numeric ID of the tube}

\item{token}{A valid API token}

\item{name}{The character name of the tube}

\item{remarks}{An updated description for the tube}

\item{barcode}{An updated barcode for the tube}

\item{box_id}{An updated holding box for the tube (numeric LabGuru ID)}

\item{location_in_box}{An updated box location for the tube}

\item{sample_uuid}{An updated tissue to associate with the tube}

\item{member_id}{An updated owner for the tube}
}
\description{
This function lets you update an existing tube via the LabGuru API. Values
set as \code{NULL} will not be updated.
}

