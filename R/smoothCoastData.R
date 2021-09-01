#' Smoothed coast data objects
#' 
#' See `vignette("Smooth Coastline Data")` for how these data were created.
#' 
#' @name smoothCoastData
#' @aliases buffers world sample_points buffer20 buffer300 world.wintri trim.world.wintri
#' 
#' @details
#' * `buffer300` A 300 km buffer list in `sp::SpatialPolygons`, `sp::SpatialLines`, and 
#' dataframe format in Winkel Tripel projection. A buffer line that is from a coastline
#' where the islands are removed is also included but this line will go on top of islands
#' so there will be more upwelling NAs along the coast.
#' * `buffer20` A 20 km buffer in `sp::SpatialPolygons`, `sp::SpatialLines`, and 
#' dataframe format in Winkel Tripel projection. A buffer line that is from a coastline
#' where the islands are removed is also included but this line will go on top of islands
#' so there will be more upwelling NAs along the coast.
#' * `world` A world coastline in `sp::SpatialLines`, format in Winkel Tripel projection. `trim.world.wintri` has the islands removed
#' * `sample_points` A sample of points from the smooth 20km buffer line where points 
#' are 100km apart.
#' 
#'
#' @keywords data upwelling
NULL
