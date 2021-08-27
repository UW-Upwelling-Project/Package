#' Get the mean raster values around a point
#'
#' This is applies `raster::extract()` but does some checking on the projection
#' of the inputs.
#'
#' @param p the points as a `sp::SpatialPoints` object
#' @param r the raster as a `raster::raster` object
#' @param d distance from the coastline
#' @param units what units the distance is in
#' @param crs.to.use name of the projection. must be one that
#' the **sp** package recognizes.
#'
#' @return raster value at the points
#' @keywords image
#' @export
getStatsAroundPoint <- function(p, r, d = 100, units = "km", crs.to.use = crs.wintri, fun = "mean") {
  # make sure the units are right
  crs.to.use <- checkunits(crs.to.use, units)$crs
  if (!inherits(p, "SpatialPoints")) stop("pts should be a SpatialPoints object")
  if (!inherits(r, "raster")) stop("r should be a raster")
  mpts <- p
  if (!identical(crs(mpts), crs.to.use)) mpts <- sp::spTransform(mpts, crs.to.use)
  mras <- r
  if (!identical(crs(mras), crs.to.use)) mras <- raster::projectRaster(mras, crs = crs.to.use, over = TRUE)
  circle_pt <- raster::buffer(mpts, width = d)
  vals <- raster::extract(mras, circle_pt)
  val <- c()
  for (i in 1:length(vals)) val <- c(val, do.call(fun, vals[[i]], na.rm = TRUE))
  return(val)
}

checkunits <- function(x, units = "m") {
  if (stringr::str_detect(x, "proj=longlat")) stop("units only applicable for projection in meters")
  x <- stringr::str_trim(stringr::str_split(x, " ")[[1]])
  u <- stringr::str_split(x[stringr::str_detect(x, "units")], "=")
  if (!length(u) == 0) {
    u <- u[[1]][2]
    if (identical(u, units)) {
      return(x)
    }
  } else {
    u <- units
  }
  x <- paste(x[!stringr::str_detect(x, "units")], collapse = " ")
  x <- paste0(x, " +units=", u)
  return(list(crs = x, units = u))
}
