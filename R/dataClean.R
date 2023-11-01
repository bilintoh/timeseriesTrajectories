#' The function allows users to reclassify their raster files.
#' @param x is the data which must be a raster stack
#' @param reclassify indicates whether the user wants to reclassify the raster data or not. default set to "no"
#' @param classifyDf is a data frame containing the range of number to be reclassified and their corresponding new values. Default is NULL.
#' @importFrom terra classify
#' @return The output from \code{\link{dataClean}}
#' @export
#'
#'
dataClean <- function(x,
                      reclassify = 'no',
                      classifyDf = NULL){
  # Check the data type
  if (!class(x) %in% c("SpatRaster","data.frame")){
    stop("This function is intended for SpatRastes and dataframes only", call. = FALSE)
  }
  if (!reclassify %in% c('no','yes')){
    stop("classify must have a yes or no input", call. = FALSE)}

  else if (class(x) %in% c("SpatRaster")){
    if (reclassify %in% ('no')){
      x <- x
    } else if (reclassify %in% ('yes')){
      x <- terra::classify(x,classifyDf)
    }
  }
  return(x)
}
