% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_package_database.weather}
\alias{load_package_database.weather}
\title{Load weather data.}
\usage{
load_package_database.weather(
  input.folder,
  overwrite.db = c("prompt", "yes", "no"),
  verbose = TRUE
)
}
\arguments{
\item{input.folder}{File path. Folder containing all input files.}

\item{overwrite.db}{Default behaviour if db.name already exists. yes: Overwrite old database file. no: Generate new db.name with todays date and version number. prompt: User is prompted to choose a behaviour. If function is run non-interactively (e.g. on the cluster) a default behaviour (TRUE or FALSE) \strong{must} be supplied.}

\item{verbose}{Logical. Should the function generate messages (default: TRUE)}
}
\description{
If no hyenaR database is present, will create a standalone .database object
for weather. This will function in a similar way to hyenaR processes.
}
\details{
If a hyenaR database is present, will append weather data to the database
allowing weather and hyena information to be used simultaneously.
}
