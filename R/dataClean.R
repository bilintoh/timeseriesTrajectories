#' The function allows users to reclassify their raster files.
#' @param x is the data which must be a raster stack
#' @param classifyDf is a data frame containing the range of number to be reclassified and their corresponding new values. Default is NULL.
#' @importFrom terra classify
#' @return The output from \code{\link{dataClean}}
#' @export
#'
#'
dataClean <- function(x,
                      classifyDf = NULL){
  # Check the data type
  if (!class(x) %in% c("SpatRaster","data.frame")){
    stop("This function is intended for SpatRastes only", call. = FALSE)
  }
  x <- terra::classify(x,classifyDf)
  return(x)
}
