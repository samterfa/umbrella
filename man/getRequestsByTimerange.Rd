% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReportingAPI.R
\name{getRequestsByTimerange}
\alias{getRequestsByTimerange}
\title{Requests by Timerange}
\usage{
getRequestsByTimerange(
  type,
  from,
  to,
  domains = NULL,
  urls = NULL,
  categories = NULL,
  policycategories = NULL,
  ip = NULL,
  ports = NULL,
  identityids = NULL,
  identitytypes = NULL,
  applicationid = NULL,
  verdict = NULL,
  securityoverridden = NULL,
  bundleid = NULL,
  threats = NULL,
  threattypes = NULL,
  ampdisposition = NULL,
  antivirusthreats = NULL,
  offset = 0,
  limit = 100,
  organizationId = Sys.getenv("umbrellaOrganizationId")
)
}
\arguments{
\item{type}{the type of entries used to calculate the result (dns, proxy, firewall)}

\item{from}{timestamp to filter from (or relative, e.g. -1days)}

\item{to}{timestamp to filter to  (or relative, e.g. now)}

\item{domains}{domain filter, comma delimited}

\item{urls}{url filter, comma delimited}

\item{categories}{categories filter, comma delimited ints}

\item{policycategories}{policy-triggering categories filter, comma delimited ints}

\item{ip}{ip filter}

\item{ports}{81 - port filter, comma delimited ports and ranges}

\item{identityids}{identity filter, comma delimited}

\item{identitytypes}{identity type filter, comma delimited.}

\item{applicationid}{application id filter}

\item{verdict}{verdict filter, comma delimited}

\item{securityoverridden}{whether security was overridden for this request}

\item{bundleid}{proxy bundle ID}

\item{threats}{threat names filter, comma delimited}

\item{threattypes}{threat types filter, comma delimited}

\item{ampdisposition}{AMP disposition filter, comma delimited}

\item{antivirusthreats}{threat names caught by antivirus, comma delimited}

\item{offset}{offset}

\item{limit}{limit}

\item{organizationId}{the organization id}
}
\description{
Gets activity volume within timeframe
}
\concept{ReportingAPI}
