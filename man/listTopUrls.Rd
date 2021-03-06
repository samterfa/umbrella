% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReportingAPI.R
\name{listTopUrls}
\alias{listTopUrls}
\title{Top URLs}
\usage{
listTopUrls(
  from,
  to,
  domains = NULL,
  securityoverridden = NULL,
  bundleid = NULL,
  ampdisposition = NULL,
  sha256 = NULL,
  antivirusthreats = NULL,
  offset = 0,
  limit = 100,
  organizationId = Sys.getenv("umbrellaOrganizationId")
)
}
\arguments{
\item{from}{timestamp to filter from (or relative, e.g. -1days)}

\item{to}{timestamp to filter to  (or relative, e.g. now)}

\item{domains}{domain filter, comma delimited}

\item{securityoverridden}{whether security was overridden for this request}

\item{bundleid}{proxy bundle ID}

\item{ampdisposition}{AMP disposition filter, comma delimited}

\item{sha256}{sha256 filter}

\item{antivirusthreats}{threat names caught by antivirus, comma delimited}

\item{offset}{offset}

\item{limit}{limit}

\item{organizationId}{the organization id}
}
\description{
Returns a list of top-N URLs proxied for a given domain.
}
\concept{ReportingAPI}
