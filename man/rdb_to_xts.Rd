% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdb_to_xts.R
\name{rdb_to_xts}
\alias{rdb_to_xts}
\title{Transform the data.table object into a xts object}
\usage{
rdb_to_xts(
  x,
  needed_columns = c("period", "series_code", "series_name", "value"),
  series_columns = c("series_code", "series_name")
)
}
\arguments{
\item{x}{\code{data.table}. The \code{data.table} returned by the \code{rdb}
function.}

\item{needed_columns}{Vector of character strings (default
\code{c("period", "series_code", "series_name", "value")}). Vector of column
names which are needed to transform the \code{data.table} into a \code{xts}
object.}

\item{series_columns}{Vector of character strings (default
\code{c("series_code", "series_name")}). Vector of series column
names.}
}
\value{
A \code{xts} object.
}
\description{
For some analysis, it is more convenient to have a \code{xts} object
instead of a \code{data.table} object.
}
\examples{
\dontrun{
library(xts)
library(data.table)
library(rdbnomics)

df <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")
rdb_to_xts(df)
}
}
\seealso{
\code{\link{rdb}}, \code{\link{rdb_rename_xts}}
}
\author{
Sebastien Galais
}
