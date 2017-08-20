
#' Title fars_read : this function check for a filename in input if the file exists. If no returns an error else read the csv file et returns the dataset
#'
#' @param filename the csv file to read
#'
#' @return a dataframe from the input file or an error if file does not exit
#' @export
#'
#' @examples fars_read("mydir/myfile.csv")
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Title make_filename : this function returns a file name for an inpu string year
#'
#' @param year the year to insert when calling sprintf to get the filename of the dataset needed
#'
#' @return a character vector combing the input year in the string accident_%d.csv.bz2 instead of %d to get a filename corresponding to the input year
#' @export
#'
#' @examples make_filename(2017)
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Title fars_read_years : this function read for one more many years the accident files and then select months and years
#'
#' @param years a vector with the years for whcich we want to loop
#'
#' @return foreach year in the input vector years the months found in the dataframe read with make_filename, a warning message with the invalid year and NULL returned for that invalid year if an error occurs
#' @export
#' @importFrom magrittr
#'
#' @examples fars_read_years(c(2001,2017))
#'
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


#' Title fars_summarize_years : this function read and summarize for on or more years input, the accidents group by year and month
#'
#' @param years a vector with the years for whcich we want to loop
#'
#' @return call fars_read_years for the input vector years and for the returned year-month will group by year and call summarize to get summary data related to each
#' @export
#'
#' @importFrom magrittr
#'
#' @examples fars_summarize_years(c(2001,2017))
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' Title fars_map_state : this function designs a graphic plot for the year and the state input from dataset read in accident file
#'
#' @param state.num an input state to filter on the accidents
#' @param year the input year on which to get the accidents dataset to plot
#'
#' @return a graphic plot with the accidents related to the inputs year and state, read in the dataset returned by fars_read called with that year and then filtered on the input state
#' @export
#'
#' @examples fars_map_state(1,2017)
#'
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
