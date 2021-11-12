#' Autodetection algorithm 3
#'
#' Take a data frame processed from data downloaded from ERDDAP and evaluates
#' upwelling by comparing the SST at a point along a smoothed coast to the average
#' SST at a point offshore. This uses a smoothed coastline, smoothed offshore line,
#' and coastal sample points that are part of the **imageML** package. See \code{\link{smoothCoastData}}.
#'
#' @param x cleaned dataframe of SST values. See \code{\link{processOIData}()}. Note, this function might require that lon_0=0 in the crs.
#' @param threshold offshore-coast difference for upwelling
#' @param val what the SST column is called
#' @param p the coastal points as a `sp::SpatialPoints` object. Default is to use `km100` in `data("sample_points)"` for the coastline where the islands are not removed.
#' @param l.offshore (optional) the offshore line as a `sp::SpatialLines` object.
#' @param d.offshore the radius of the circle to average around the offshore points.
#' @param l.coast (optional) the coast line as a `sp::SpatialLines` object.
#' @param d.coast the radius along the coast to average around the coast points. Operates only along the coast line.
#' @param smooth.method (optional) the smoothing to apply to the offshore line
#' @param na.rm whether to remove NAs in the raster when calculating the offshore SST. Used to deal with islands.
#' @param ... extra smoothing parameters to pass to \code{\link{getNearestPointOnLine}()}.
#'
#' @details This expects that only one date is in the dataframe. Run this code to
#' filter the dataframe by date.
#' `df_processed <- x %>% dplyr::filter(date == custom_date)`
#'
#' The coastal points and offshore line are saved data objects in **imageryML**. See \code{vignette("Smooth Coastline Data")} for how these data were created.
#'
#' @return dataframe with upwelling TRUE/FALSE added
#' @keywords auto
#' @export
autoDetect3 <- function(x, threshold = 2, val = "sst",
                        p = imageryML::sample_points$wintri$km100,
                        smooth.method = "ksmooth",
                        l.offshore = imageryML::buffer300$wintri$line,
                        d.offshore = 50,
                        l.coast = imageryML::buffer20$wintri$line,
                        d.coast = 0,
                        na.rm = TRUE, ...) {

  # Error-checking
  if (!all(c("lon", "lat", val) %in% colnames(x))) stop(paste("missing either lat, lon or,", val))
  if (x$lat %>% stats::na.omit() %>% unique() %>% sort() %>% diff() %>% min() != 0.25) stop("this function only works if the grid is 0.25 in latitude")
  # check that only one date is included
  if ("date" %in% colnames(x)) {
    if (x %>% dplyr::select("date") %>% unique() %>% length() > 1) {
      stop("There is more than one data in the data frame")
    }
  }
  if (stringr::str_detect(raster::crs(p), "longlat")) stop("the coastal points need to be in a meters projection not longlat.")
  # End error-checking

  x$val <- x[, val]

  r <- x %>%
    dplyr::select("lon", "lat", "val") %>%
    raster::rasterFromXYZ(crs = "+proj=longlat")
  r <- raster::projectRaster(r, crs = raster::crs(p))

  coast.pts <- raster::crop(p, raster::extent(r))
  offshore.pts <- getNearestPointOnLine(l.offshore, coast.pts, smooth.method = smooth.method, ...)$sp
  off.sst <- getStatsAroundPoint(offshore.pts, r, d = d.offshore, na.rm = na.rm)
  if (d.coast == 0) {
    coast.sst <- getStatsAlongLine(r, coast.pts, d = 0, na.rm = na.rm)
  } else {
    coast.sst <- getStatsAlongLine(r, coast.pts, l = l.coast, d = d.coast, na.rm = na.rm)
  }
  df <- data.frame(coast.pts@coords,
    offshore = off.sst, coast = coast.sst,
    upwelling = (off.sst - coast.sst) >= threshold
  )
  colnames(df) <- c("x", "y", "offshore.SST", "coast.SST", "upwelling")

  return(list(
    df = df, offshore.pts = offshore.pts, coast.pts = coast.pts,
    threshold = threshold, d.offshore = d.offshore,
    d.coast = d.coast, smooth.method = smooth.method, ...
  ))
}
