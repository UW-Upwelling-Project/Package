#' Autodetection algorithm 1
#'
#' Take a data frame processed from data downloaded from ERDDAP and evaluates upwelling at a specific date.
#'
#' @param x cleaned dataframe. See \code{\link{processOIData}()}.
#' @param threshold offshore-coast difference for upwelling
#' @param val what the SST column is called
#' 
#' @details This expects that only one date is in the dataframe. Run this code to
#' filter the dataframe by date.   
#' `df_processed <- x %>% dplyr::filter(date == custom_date)`
#' 
#' @return dataframe with upwelling TRUE/FALSE added
#' @keywords auto
#' @export
autoDetect1 <- function(x, threshold = 2, val="sst") {
  
  # Error-checking
  if(!all(c("lon", "lat", val) %in% colnames(x))) stop(paste("missing either lat, lon or,", val))
  if(x$lat %>% stats::na.omit %>% unique %>% sort %>% diff %>% min != 0.25) stop("this function only works if the grid is 0.25 in latitude")
  # check that only one date is included
  if("date" %in% colnames(x)){
    if(x %>% dplyr::select("date") %>% unique %>% length > 1)
      stop("There is more than one data in the data frame")
  }
  # End error-checking
  
  x$val <- x[, val]
  
  # Automatic Detection Method
  # Find SST values next to land (sst_coast_X) and check if upwelling
  is_upwelling <- x %>% 
    dplyr::group_by(.data$lat) %>% 
    # 1 longitude tick away from land
    dplyr::mutate(coast_1 = dplyr::last(stats::na.omit(.data$val)),
           # 4 longitude ticks away from land (1 degree in longitude)
           coast_2 = dplyr::nth(stats::na.omit(.data$val), -5),
           # 8 longitude ticks away from land (2 degrees in longitude)
           coast_3 = dplyr::nth(stats::na.omit(.data$val), -9),
           # 12 longitude ticks away from land (3 degrees in longitude)
           coast_4 = dplyr::nth(stats::na.omit(.data$val), -13)) %>% 
    # Find difference between pixels
    dplyr::summarize(is_upwelling_1_2 = .data$coast_2 - .data$coast_1 > threshold,
              is_upwelling_1_3 = .data$coast_3 - .data$coast_1 > threshold,
              is_upwelling_1_4 = .data$coast_4 - .data$coast_1 > threshold) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$lat) %>% 
    # Check if any upwelling in each latitude
    # first() because I want to return one row for each lat
    dplyr::summarise(is_upwelling_total = dplyr::first(.data$is_upwelling_1_2 | .data$is_upwelling_1_3 | .data$is_upwelling_1_4)) %>% 
    dplyr::ungroup()
  
  final_df <- x %>% 
    dplyr::left_join(is_upwelling) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$lat) %>% 
    # Find  (coast) longitude corresponding to row preceding last Na/NaN value in SST
    dplyr::mutate(last_lon = dplyr::across(.data$val, ~ tail(.data$lon[!is.na(.)], 1))) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(last_lon = .data$last_lon$val)
  
    return(final_df)
}