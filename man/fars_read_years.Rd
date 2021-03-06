% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read in a given year of fars data}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector or list of years. Each value should be a character, integer or numeric}
}
\value{
A list of data frames by year with the month and year
}
\description{
This function inputs a list of years, reads in the data and outputs a list of data frames with the month and year
}
\details{
Warning: The relevant fars files need to be in the working directory with the format:
accident_YEAR.csv.bz2 where YEAR is a placeholder for a 4-digit year
}
\examples{
\dontrun{data_2013 <- fars_read_years(2013)}
\dontrun{data_2013_to_2015 <- fars_read_years(c(2013,2014,2015))}
}
