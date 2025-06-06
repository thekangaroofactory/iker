% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_POSIXct.R
\name{to_POSIXct}
\alias{to_POSIXct}
\title{Convert ISO-8601 Character Columns to POSIXct}
\usage{
to_POSIXct(data, names_ct)
}
\arguments{
\item{data}{a data.frame}

\item{names_ct}{the names of the columns to convert}
}
\value{
a data.frame with converted columns
}
\description{
Convert ISO-8601 Character Columns to POSIXct
}
\examples{
to_POSIXct(data.frame(a = "2025-05-13T12:15:30+0200"), "a")
}
