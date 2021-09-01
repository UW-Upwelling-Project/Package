#' Process Optimal Interpolation data
#' 
#' @description
#' Process a download from CoastWatch ERDDAP OI SST data. Mask out the Puget Sound
#' 
#' @details
#' 
#' download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst%5B(2019-01-01T12:00:00Z):1:(2020-01-01T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(35.125):1:(52.125)%5D%5B(228.125):1:(238.125)%5D", "data/pilot_data.csv")
#'  
#' @param inputfile a csv file like downloaded above
#' 
#' @return return a processed data frame
#' @keywords data
#' @export
processOIData <- function(inputfile) {
  df_raw <- readr::read_csv(inputfile)
  # Process data
  df_processed <- df_raw %>% 
    # Get rid of miscellaneous first row
    slice(-1) %>% 
    # zlev is a column of zeroes, so get rid of that
    dplyr::select(-zlev) %>% 
    # Convert into date
    dplyr::mutate(time = lubridate::ymd_hms(time)) %>% 
    # Set column names
    dplyr::rename(date = time,
                  lat = latitude,
                  lon = longitude) %>% 
    # Convert date column to Date type
    dplyr::mutate(date = as.Date(date),
                  lat = as.numeric(lat),
                  lon = as.numeric(lon),
                  sst = as.numeric(sst))
  
  # mask out Puget Sound, Strait of Juan de Fuca and Georgia Strait
  masks <- list(c(235.4488, 236.884, 47.87651, 50.13138),
                c(232.2913, 233.8987, 50.28689, 51.60871),
                c(234.4154, 235.9654, 49.04283, 50.09251))
  
  for (m1 in masks) {
    # index of Puget Sound, Strait of Juan de Fuca or Georgia Strait
    mask_loc <- df_processed$lat <= m1[4] & df_processed$lat >= m1[3] &
      df_processed$lon <= m1[2] & df_processed$lon >= m1[1]
    # Change to NA
    df_processed$sst[mask_loc] <- NA
  }
  df_processed <- df_processed %>%
    mutate(lon = lon - 360)
  
  return(df_processed)
}