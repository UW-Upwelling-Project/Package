#' Get or convert the units from a projection string
#'
#' Utility function to return the units.
#'
#' @param x a projection string
#' @param units the units you want to convert to
#'
#' @return list with crs with units added and units
#' @keywords image
#' @export
checkunits <- function(x, units="m") {
  if (stringr::str_detect(x, "proj=longlat")){
    if(!missing(units) && units!="degree") stop("crs is longlat and units must be degrees")
    return(list(crs = x, units = "degree"))
  }
  x <- stringr::str_trim(stringr::str_split(x, " ")[[1]])
  u <- stringr::str_split(x[stringr::str_detect(x, "units")], "=")
  if (!length(u) == 0) {
    u <- u[[1]][2]
  } else {
    u <- units
  }
  x <- paste(x[!stringr::str_detect(x, "units")], collapse = " ")
  x <- paste0(x, " +units=", u)
  return(list(crs = x, units = u))
  return(u)
}