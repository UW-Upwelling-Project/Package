#' Download data from an ERDDAP server
#' 
#' @description
#' Download data from CoastWatch ERDDAP server ncdcOisst21Agg data which is daily
#' optimal interpolation SST data.
#' 
#'  
#' @param start_date first date
#' @param stop_date final date
#' @param min_lat minimum latitude
#' @param max_lat maximum latitude
#' @param min_lon minimum longitude
#' @param max_lon maximum longitude
#' @param silent no messages to console
#' 
#' @return The function saves a csv file and returns the data list invisibly.
#' @author Howard Baek
#' @keywords data
#' @export
import_erddap <- function(start_date, stop_date, min_lat, max_lat, min_lon, max_lon, silent=FALSE) {
  
  url <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst%5B(",
                start_date, "T12:00:00Z):(", stop_date, "T12:00:00Z)%5D%5B(0.0)%5D%5B(", 
                min_lat, "):(", max_lat, ")%5D%5B(",
                min_lon, "):(", max_lon, ")%5D")
  
  if(!silent) print("Reading data from url.........")
  
  df <- readr::read_csv(url)
  
  if(!silent) print(".........DONE!!!")
  
  file_name <- paste0("inst/extdata/erddap_", start_date, "_", stop_date,
                      "_", "lat_", min_lat, "_", max_lat,
                      "_lon_", min_lon, "_", max_lon, ".csv")
  
  # Write to csv
  df %>% 
    readr::write_csv(file_name)
  
  # User message
  msg <- paste0("Data saved to working directory as ", file_name)
  if(!silent) print(msg)
}