% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check_function_arg.station}
\alias{check_function_arg.station}
\title{Check weather station names are valid.}
\usage{
check_function_arg.station(station)
}
\arguments{
\item{station}{A character vector. Vector of weather station names.}
}
\value{
A character vector of weather station names
}
\description{
Check weather station names are valid.
}
\examples{
#Will work
check_function_arg.station("mvua")

#Won't work
#check_function_arg.station("not_a_station")
}
