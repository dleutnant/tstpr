% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tstpr_download.R
\name{get_timeseries}
\alias{get_timeseries}
\title{get_timeseries}
\usage{
get_timeseries(con, version, parameter, site, from = NULL, to = NULL,
  parallel = FALSE, cpu = 1, verbose = F, debug = FALSE)
}
\arguments{
\item{con}{A tstp.connection object.}

\item{version}{Query parameter (optional).}

\item{parameter}{Query parameter (optional).}

\item{site}{Query parameter (optional).}

\item{from}{Specifies the first date (\%Y-\%m-\%d).}

\item{to}{Specifies the last date (\%Y-\%m-\%d).}

\item{parallel}{logical. Loads timeseries data in parallel.}

\item{cpu}{Number of CPU's to be used.}

\item{verbose}{logical. Should informative outputs printed during function evaluation?}

\item{debug}{logical. For debugging purposes only.}
}
\description{
Get multiple time series data from a tstp server
}
\details{
This function is basically a wrapper for \code{tstp_get} to get multiple time series with one call.
It creates xts-objects for each time series in the current R environment with 'site.parameter.version' syntax.
R objects with identical names are overwritten without warning.
}
\author{
Dominik Leutnant
}
