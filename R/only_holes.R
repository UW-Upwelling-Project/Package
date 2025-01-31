#' Keep only holes in polygons
#' 
#' Adapted from **spatialEco** function `remove.holes()`
#' 
#' @param x A `sp::SpatialPolygons` object.
#' 
#' @export
#' @keywords image
only.holes <- function (x) 
{
  if (!any(which(utils::installed.packages()[, 1] %in% "maptools"))) 
    stop("please install maptools package before running this function")
  xp <- methods::slot(x, "polygons")
  holes <- lapply(xp, function(x) sapply(methods::slot(x, "Polygons"), 
                                         methods::slot, "hole"))
  res <- lapply(1:length(xp), function(i) methods::slot(xp[[i]], 
                                                        "Polygons")[holes[[i]]])
  IDs <- row.names(x)
  x.fill <- sp::SpatialPolygons(lapply(1:length(res), function(i) sp::Polygons(res[[i]], 
                                                                               ID = IDs[i])), proj4string = sp::CRS(sp::proj4string(x)))
  methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, 
                                                            "polygons"), maptools::checkPolygonsHoles)
  methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, 
                                                            "polygons"), "comment<-", NULL)
  pids <- sapply(methods::slot(x.fill, "polygons"), function(x) methods::slot(x, "ID"))
  x.fill <- sp::SpatialPolygonsDataFrame(x.fill, data.frame(row.names = pids, 
                                                            ID = 1:length(pids)))
  return(x.fill)
}