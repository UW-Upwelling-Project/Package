#' Find nearest point to lines
#'
#' Lines are specified as a `sp::SpatialLines` object. The points are a 
#' `sp::SpatialPoints` object. If there are multiple lines in the lines
#'  object, the nearest point might be wrong so results should be checked.
#'
#' Note, the `maptools::snapPointsToLines()` function should do this, but it is
#' not working with the `SpatialLines` object produced by converting a polygon
#' to lines. It may work fine if one converts the `SpatialPolygon` to
#' a `SpatialLinesDataFrame` with the proper line id's. 
#'
#' @param p the points as a `sp::SpatialPoints` object.
#' @param l the lines as a `sp::SpatialLines` object.
#' @param smooth.method type of method to use to smooth the offshore line. This is needed to have the offshore points spread regularly along the line and not be affected by divits.
#' @param ... parameters to specify smoothing. If "gSimplify", `tol=30` is ok. If "ksmooth", `smoothness=5` is ok. 
#' 
#' @details If the line is wavey, smoothing can help find points that look more
#' perpendicular to the coast.
#'
#' @return vector of points
#' @keywords auto
#' @export
getNearestPointOnLine <- function(l, p, smooth.method = c("ksmooth", "none", "gSimplify"), ...) {
  smooth.method <- match.arg(smooth.method)
  extras <- list(...)
  ### Check that inputs are right format and convert as needed
  if (!inherits(p, "SpatialPoints")) stop("p must be SpatialPoints")
  if (!inherits(l, "SpatialLines")) stop("l must be SpatialLines")
  crs.l <- raster::crs(l)
  if(smooth.method=="gSimplfy" && is.null(extras$tol)) extras$tol <- 30
  if(smooth.method=="ksmooth" && is.null(extras$smoothness)) extras$smoothness <- 5
  l <- switch(smooth.method,
         none = l,
         gSimplify = rgeos::gSimplify(l, tol=extras$tol),
         ksmooth = smoothr::smooth(l, method="ksmooth", smoothness=extras$smoothness)
  )
  # Convert l to dataframe
  if (inherits(l, "SpatialLines")) {
    df <- c()
    n <- length(l@lines[[1]]@Lines)
    for (i in 1:n) df <- rbind(df, cbind(l@lines[[1]]@Lines[[i]]@coords, ID = i))
    l <- as.data.frame(df)
  }
  if (!inherits(l, "data.frame")) stop("Something is wrong with the lines object.")
  if (ncol(l) == 1) stop("Something is wrong with the lines object. It needs x and y coordinates.")
  if (ncol(l) == 2) l$id <- 1
  if (any(is.na(l[, 3]))) stop("Something is wrong with the lines object. The ID column has NAs.")
  # At this point l is a dataframe with 3 columns
  ### End input checking

    pts <- p@coords
    close_coords <- matrix(NA, nrow(pts), 2)
    for (i in 1:nrow(pts)) {
      close_coords[i, ] <- maptools::nearestPointOnLine(df, pts[i, ])
    }
    close_sp <- sp::SpatialPoints(close_coords, proj4string = crs.l)
    return(list(coords = close_coords, sp = close_sp))
  }
