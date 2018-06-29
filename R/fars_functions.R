#' Creates and returns a data frame from a CSV file
#'
#' Will throw an error if no file matches the filename
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @param filename character string of the name to the file
#' @return a data.frame object
#' @examples
#' fars_read(system.file("/extdata/accident_2015.csv.bz2",package="CourseraPackage"))
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Returns a string that can be a name to a CSV file, where the input \code{year} will
#' be part of the string
#'
#' Function will throw an error if \code{year} is not an integer
#'
#' @param year a number representing a year
#' @return a string of the pattern: "accident_{year}.csv.bz2"
#' @examples
#' make_filename(2018)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file(sprintf("/extdata/accident_%d.csv.bz2", year),package="CourseraPackage")
#  sprintf("accident_%d.csv.bz2", year)
}

#' Will process a group of csv files in the directory
#'
#' files must be in current working directory since the name make_filename
#' function does not generate paths but rather only the name of file
#' @importFrom dplyr mutate select %>%
#' @param years either a vector or list of integers representing years
#' @return a list of data.frame objects read in. List may contain NULL if
#' a file with an input year does not exist
#' @examples
#' fars_read_years(c(2013,2014,2015))
#' @export
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

#' Creates one summary output from all CSV files with the inputed years
#'
#' Will first merge all csv files and then summarise the resulting single data frame.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @param years either a vector or list of integers representing years
#' @return a matrix where columns represent year and rows represent months
#' and the value of each cell is the total number of occurrences
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Will plot accidents as points on a map of the state by Longitude and Latitude
#'
#' The location of the accidents are captured by Longitude and Latitude fields.
#' These fields will be used to plot the accident locations on a map of the
#' state in which they occur. Errors will be thrown if the inputted \code{state.num}
#' or \code{year} is not valid
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @param state.num The unique integer value that identifies a state
#' @param year An integer value that represents a year
#' @return a plot of accident locations on a map of the state in which the accident
#' occurred
#' @examples
#' fars_map_state(40, 2013)
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
