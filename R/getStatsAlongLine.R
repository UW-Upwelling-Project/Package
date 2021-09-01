#' Get the mean raster values along a line
#'
#' This is applies `raster::extract()` but does some checking on the projection
#' of the inputs. Set `d=0` to get value at the points.
#'
#' @param p the points as a `sp::SpatialPoints` object
#' @param r the raster as a `raster::raster` object
#' @param l the lines as a `sp::SpatialLines` object if `d` is not 0. 
#' @param d distance to average along the line. Distance is in the units of the points and raster objects
#' @param na.rm whether to remove NAs in the raster when computing the average
#' 
#' @details If there are multiple lines within the radius of `d`, then the average is over all lines covered
#'  in that radius. The segments to average is determine by a circle around the points.
#' 
#' @seealso \code{\link{checkunits}()}
#'
#' @return raster values around the points
#' @keywords auto
#' @export
getStatsAlongLine <- function(r, p, l=NULL, d=0, na.rm=TRUE) {
  if(d==0){
    vals <- raster::extract(r, p)
    return(vals)
  }
  if(is.null(l)) stop("If d is not zero, need the coastline.")
  pts <- p@coords
  vals <- c()
  for(i in 1:nrow(pts)){
    pt <- sp::SpatialPoints(pts[i,,drop=FALSE])
    circle_pt <- raster::buffer(pt, width = d)
    segment_pt <- raster::intersect(l, circle_pt)
    vals <- c(vals, mean(raster::extract(r, segment_pt)[[1]], na.rm=na.rm))
  }
  return(vals)
}