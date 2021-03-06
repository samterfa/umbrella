% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ManagementAPI.R
\name{modifyInternaldomain}
\alias{modifyInternaldomain}
\title{Update an InternalDomain}
\usage{
modifyInternaldomain(
  internalDomainId,
  domain,
  description = NULL,
  includeAllVAs = NULL,
  includeAllMobileDevices = NULL,
  organizationId = Sys.getenv("umbrellaOrganizationId")
)
}
\arguments{
\item{internalDomainId}{The internal domain id related to this InternalDomain}

\item{domain}{internal domain}

\item{description}{internal domain description}

\item{includeAllVAs}{apply internal domain to all virtual appliances}

\item{includeAllMobileDevices}{apply internal domain to all mobile devices}

\item{organizationId}{The organization id related to this InternalDomain}
}
\description{
Update an InternalDomain
}
\concept{ManagementAPI}
