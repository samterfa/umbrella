% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ManagementAPI.R
\name{createUser}
\alias{createUser}
\title{Create a new User}
\usage{
createUser(
  firstname,
  lastname,
  email,
  password,
  roleId,
  timezone,
  organizationId = Sys.getenv("umbrellaOrganizationId")
)
}
\arguments{
\item{firstname}{user first name}

\item{lastname}{user last name}

\item{email}{user email}

\item{password}{user password}

\item{roleId}{role id}

\item{timezone}{user timezone name}

\item{organizationId}{The organizationID related to this User}
}
\description{
Create a new User
}
\concept{ManagementAPI}
