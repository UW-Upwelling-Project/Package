#' Get the mean raster values around a point
#'
#' This is applies `raster::extract()` but does some checking on the projection
#' of the inputs.
#'
#' @param p the points as a `sp::SpatialPoints` object
#' @param r the raster as a `raster::raster` object
#' @param crs.to.use name of the projection. must be one that
#' the **sp** package recognizes.
#' 
#' @return raster value at the points
#' @keywords image
#' @export
getValueAtPoint <- function(p, r, crs.to.use = crs.wintri) {
  if (!inherits(p, "SpatialPoints")) stop("p should be a SpatialPoints object")
  if (!inherits(r, "raster")) stop("r should be a raster")
  if (!identical(crs(p), crs.to.use)) mpts <- sp::spTransform(p, crs.to.use)
  mras <- r
  if (!identical(crs(r), crs.to.use)) mras <- raster::projectRaster(r, crs = crs.to.use, over = TRUE)
  
  val <- raster::extract(mpts, mras)
  return(val)
}
