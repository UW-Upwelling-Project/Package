#' Make a coastline buffer
#'
#' Make a line that is parallel to the world's coastlines at a specific distance. See \code{vignette("Smooth Coastline Data")} for how these data were created for the package.
#'
#' @param d distance from the coastline
#' @param units what units the distance is in
#' @param crs.to.use name of the projection. must be one that 
#' the **sp** package recognizes
#' @param remove.holes TRUE/FALSE. If TRUE (default), the interior 
#' polygons are removed
#' 
#' @return a list with a SpatialPolygons, SpatialLines and dataframe object
#' @keywords auto
#' @export
makeCoastBuffer <- function(d = 300, units = "km", crs.to.use = "wintri", remove.holes = TRUE) {
  world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sp")
  world <- rgeos::gUnaryUnion(world)
  newcrs <- paste0("+proj=", crs.to.use, " +lon_0=0 +lat_1=0 +x_0=0 +y_0=0 +datum=WGS84 +units=", units, " +no_defs")
  mworld <- sp::spTransform(world, newcrs)
  buff1 <- rgeos::gBuffer(mworld, width = d, byid = TRUE)
  e <- raster::erase(buff1, mworld)
  if (remove.holes) e <- spatialEco::remove.holes(spatialEco::remove.holes(e))
  el <- methods::as(mworld, "SpatialLines")
  df <- c()
  n <- length(el@lines[[1]]@Lines)
  for (i in 1:n) {
    df <- rbind(df, cbind(el@lines[[1]]@Lines[[i]]@coords, ID = i))
  }
  return(list(polygon = e, line = el, df = df, crs = newcrs))
}

