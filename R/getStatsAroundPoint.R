#' Get the mean raster values around a point
#'
#' This is applies `raster::extract()` but does some checking on the projection
#' of the inputs. Set `d=0` to get value at the points.
#'
#' @param p the points as a `sp::SpatialPoints` object
#' @param r the raster as a `raster::raster` object
#' @param d distance to average around the point. Distance is in the units of the points and raster objects
#' @param na.rm whether to remove NAs in the raster when computing the average
#' 
#' @seealso \code{\link{checkunits}()}
#'
#' @return raster values around the points
#' @keywords auto
#' @export
getStatsAroundPoint <- function(p, r, d = 100, na.rm=FALSE) {
  # make sure the units are right
  if (!inherits(p, "SpatialPoints")) stop("pts should be a SpatialPoints object")
  if (!inherits(r, "RasterLayer")) stop("r should be a raster")
  if (!identical(raster::crs(p), raster::crs(r))) stop("points and raster don't have the same projection")
  if(stringr::str_detect(raster::crs(r), "longlat")) message("raster is in longlat. you probably want projection in meters.\n")

    if(d==0){ # Get value at points
      vals <- raster::extract(r, p)
      return(vals)
    }
    # Get values in circle around points
    pts <- p@coords
    vals <- c()
    for(i in 1:nrow(pts)){
      pt <- sp::SpatialPoints(pts[i,,drop=FALSE])
      circle_pt <- raster::buffer(pt, width = d)
      vals <- c(vals, mean(raster::extract(r, circle_pt)[[1]], na.rm=na.rm))
    }
    return(vals)
  }
