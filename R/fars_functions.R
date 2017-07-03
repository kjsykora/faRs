#'Read fars file
#'
#'This function reads in a fars file and returns the data stored in an R data frame
#'
#' @param filename a character with the path to the fars file you would like to read in
#' 
#' @return This function returns an R data frame with the data from the filename parameter
#' 
#' @importFrom readr read_csv
#' 
#' @importFrom dplyr tbl_df
#' 
#' @examples
#'
#'\dontrun{fars_data <- fars_read("accident_2013.csv.bz2")}
#'\dontrun{fars_data <- fars_read("accident_2014.csv.bz2")}
#'\dontrun{fars_data <- fars_read("accident_2015.csv.bz2")}
#'
fars_read <- function(filename) {
  if(!file.exists(system.file("extdata",filename,package="faRs",mustWork=TRUE)))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(system.file("extdata",filename,package="faRs",mustWork=TRUE), progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#######################################################################################

#'Create a filename for a given year
#'
#'This function takes year as an input and creates a filename character.
#'The function only creates the name character and does not create a file.
#'
#' @param year the year the accident occurs as a character, number or integer
#'
#' @return A character with the filename character.
#'
#' @examples
#'\dontrun{make_filename(2000)}
#'\dontrun{make_filename("2005")}
#'\dontrun{make_filename(1998L)}
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#######################################################################################

#'Read in a given year of fars data
#'
#'This function inputs a list of years, reads in the data and outputs a list of data frames with the month and year
#'
#'Warning: The relevant fars files need to be in the working directory with the format:
#'accident_YEAR.csv.bz2 where YEAR is a placeholder for a 4-digit year
#'
#' @param years A vector or list of years. Each value should be a character, integer or numeric
#'
#' @return A list of data frames by year with the month and year
#'
#' @importFrom dplyr mutate select %>%
#' 
#' @examples
#'\dontrun{data_2013 <- fars_read_years(2013)}
#'\dontrun{data_2013_to_2015 <- fars_read_years(c(2013,2014,2015))}

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#######################################################################################

#'Summarize data counts by year and month
#'
#'Inputs a list or vector of years and outputs a data frame summarizing the counts of data for each year and month
#'
#' @param years A list of vectors with characters, integers or numerics representing the year
#'
#' @return A data.frame summarizing the observations in the fars data with each month as a row and each year as a column
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' 
#' 
#' @importFrom tidyr spread
#' 
#' @examples
#'fars_summarize_years(2013)
#'fars_summarize_years(c(2013,2014,2015))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#######################################################################################

#'Plots the location of each data point on a map of the state
#'
#'The function inputs the state and year and 
#'outputs a plot of all the datapoints for that state on a map of the state
#'
#' @param state.num A numeric code representing the state. Should be an integer, character or numeric
#'
#' @inheritParams make_filename
#'
#' @return Outputs a map of the state with the each fars datapoint location as a point
#'
#' @importFrom dplyr filter
#' 
#' @importFrom maps map
#' 
#' @importFrom graphics points
#'
#' @examples
#' 
#' \dontrun{fars_map_state(6,2013)}
#' \dontrun{fars_map_state(12,2014)}
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}