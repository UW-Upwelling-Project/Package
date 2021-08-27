#' Find nearest point to lines
#'
#' This will work if the lines are specified as a `sp::SpatialLines` or `sp::SpatialPolygon` object. If the latter, the object is converted to a
#' `sp::SpatialLines` object. It will also
#' work if the lines are specified in
#' a dataframe (or matrix) with x, y, and id (optional), which is a
#' coordinate matrix. The points can be a `sp::SpatialPoints` object,
#' a dataframe or matrix with each
#' row the x-y coordinates with an optional id for multiple lines or it can
#' be a vector if it is only one point.
#'
#' Note, the `maptools::snapPointsToLines()` function should do this, but it is
#' not working with the `SpatialLines` object produced by converting a polygon
#' to lines. It may work fine if one converts the `SpatialPolygon` to
#' a `SpatialLinesDataFrame` with the proper line id's.
#'
#' @param p the points as a `sp::SpatialPoints` object, dataframe, matrix or vector (one point). If a dataframe or matrix, column 1 is the x locations, column 2 is the y locations. The units and projection of the points need to
#' match the lines. If `sp::SpatialPoints` is used, this can be checked. Otherwise the user needs to ensure that they match.
#' @param l the lines as a `sp::SpatialPolygons` object, `sp::SpatialLines` object, dataframe, matrix or vector (one point).   If a dataframe or matrix, column 1 is the x locations, column 2 is the y locations. If there are multiple lines, the third column should be the id of the lines.
#' @param crs.to.use name of the projection. must be one that
#' the **sp** package recognizes.
#'
#' @details
#' The default behavior is to use the 300km coastline buffer with the
#' Winkel Tripel projection.
#'
#' @return vector of points
#' @keywords image
#' @export
getNearestPointOnLine <- function(p, l = buffer300.wintri$df, crs.to.use = buffer300.wintri$crs) {
  ### Check that inputs are right format and convert as needed
  if (inherits(p, "SpatialPoints")) pts <- pts@coords
  if (inherits(l, "SpatialPolygon")) {
    l <- as(l, "SpatialLines")
  }
  if (inherits(l, "SpatialLines")) {
    df <- c()
    n <- length(l@lines[[1]]@Lines)
    for (i in 1:n) df <- rbind(df, cbind(l@lines[[1]]@Lines[[i]]@coords, ID = i))
    l <- df
  }
  if (inherits(l, "matrix")) l <- as.data.frame(l)
  if (!inherits(l, "data.frame")) stop("Something is wrong with the lines object.")
  if (ncol(l) == 1) stop("Something is wrong with the lines object. It needs x and y coordinates.")
  if (ncol(l) == 2) l$id <- 1
  if (any(is.na(l[, 3]))) stop("Something is wrong with the lines object. The ID column has NAs.")
  # At this point l is a dataframe with 3 columns
  ### End input checking

  n <- length(unique(l[, 3]))
  if (is.null(dim(p))) p <- matrix(p, nrow = 1)

  ### run through each point
  pts <- matrix(NA, nrow(p), 2)
  for (i in 1:nrow(p)) {
    df <- c()
    ## Run through each line in the lines object
    for (j in unique(l[, 3])) {
      coordsLine <- df[df[, 3] == i, 1:2]
      nearest_points <- vapply(2:nrow(coordsLine), function(x) maptools::nearestPointOnSegment(coordsLine[(x - 1):x, ], coordsPoint), FUN.VALUE = c(0, 0, 0))
      df <- rbind(df, nearest_points[, which.min(nearest_points[3, ])])
    }
    pts[i, ] <- df[1:2, which.min(df[, 3])]
  }
  pts.sp <- sp::SpatialPoints(pts, proj4string = crs.to.use)
  return(list(matrix = pts, SpatialPoints = pts.sp))
}
