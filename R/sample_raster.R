#' Sample Raster
#' 
#' @description
#' A sample raster to use in the examples
#' 
#' @details
#' Produced by this code
#' ```
#' lats <- c(40.375, 50.375)
#' lons <- c(-141.875, -120.875)
#' df_info <- rerddap::info("ncdcOisst21Agg_LonPM180")
#' df <- rerddap::griddap("ncdcOisst21Agg_LonPM180", latitude = lats, longitude = lons, time = c("2021-06-19", "2021-06-19"), fields = "sst")$data
#' df2 <- data.frame(x=df$lon, y=df$lat, z=df$sst)
#' ras <- rasterFromXYZ(df2)
#' 
#' @keywords data
"sample_raster"
