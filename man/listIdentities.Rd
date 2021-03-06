% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReportingAPI.R
\name{listIdentities}
\alias{listIdentities}
\title{Identities}
\usage{
listIdentities(
  identitytypes = NULL,
  search = NULL,
  offset = 0,
  limit = 100,
  organizationId = Sys.getenv("umbrellaOrganizationId")
)
}
\arguments{
\item{identitytypes}{identity type filter, comma delimited.}

\item{search}{a string to search for in the identities}

\item{offset}{offset}

\item{limit}{limit}

\item{organizationId}{the organization id}
}
\description{
Identities
}
\concept{ReportingAPI}
