% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R
\name{meta_summary}
\alias{meta_summary}
\alias{meta_list}
\alias{meta_update}
\alias{meta_clean}
\title{Metadata cache utilities}
\usage{
meta_summary()

meta_list(pkg = NULL)

meta_update()

meta_clean(force = FALSE)
}
\arguments{
\item{pkg}{Package names, if specified then only entries for \code{pkg}
are returned.}

\item{force}{If \code{FALSE}, then pak will ask for confirmation.}
}
\value{
\code{meta_summary()} returns a list with entries:
\itemize{
\item \code{cachepath}: absolute path of the metadata cache.
\item \code{current_db}: the file that contains the current metadata database.
It is currently an RDS file, but this might change in the future.
\item \code{raw_files}: the files that are the downloaded \verb{PACKAGES*} files.
\item \code{db_files}: all metadata database files.
\item \code{size}: total size of the metadata cache.
}

\code{meta_list()} returns a data frame (tibble) of all available
packages in the configured repositories.

\code{meta_update()} returns nothing.

\code{meta_clean()} returns nothing
}
\description{
Various utilities to inspect, update and clean the metadata cache.
See the pkgcache package if you need for control over the metadata cache.
}
\details{
\code{meta_summary()} returns a summary of the metadata cache.

\code{meta_list()} lists all (or some) packages in the metadata
database.

\code{meta_update()} updates the metadata database. You don't
normally need to call this function manually, because all pak functions
(e.g. \code{\link[=pkg_install]{pkg_install()}}, \code{\link[=pkg_download]{pkg_download()}}, etc.) call it automatically,
to make sure that they use the latest available metadata.

\code{meta_clean()} deletes the whole metadata DB.
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Metadata cache summary
meta_cummary()
\dontshow{\}) # examplesIf}
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# The current metadata DB
meta_list()
# Selected packages only
meta_list(pkg = c("shiny", "htmlwidgets"))
\dontshow{\}) # examplesIf}
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Update the metadata DB
meta_update()
\dontshow{\}) # examplesIf}
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Delete the metadata DB
meta_clean()
\dontshow{\}) # examplesIf}
}
