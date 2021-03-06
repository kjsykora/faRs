% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plots the location of each data point on a map of the state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A numeric code representing the state. Should be an integer, character or numeric}

\item{year}{the year the accident occurs as a character, number or integer}
}
\value{
Outputs a map of the state with the each fars datapoint location as a point
}
\description{
The function inputs the state and year and 
outputs a plot of all the datapoints for that state on a map of the state
}
\examples{

\dontrun{fars_map_state(6,2013)}
\dontrun{fars_map_state(12,2014)}

}
