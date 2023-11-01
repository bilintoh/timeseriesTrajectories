#' Create data for number of presence and change.
#' @param x is the data raster stack.
#' @param nodata is the interger that shows NA in the raster stack.
#' @import terra
#' @return The output from \code{\link{presenceData}}
#' @export
presenceData <- function(x,
                        nodata = -999){
  # Set NA
  terra::NAflag(x) <- nodata
  reclass_df <- cbind(from = c(-Inf),
                      to = c(0),
                      becomes = c(1))
  reclass_df2 <- cbind(from = c(0),
                       to = c(Inf),
                       becomes = c(1))
  reclassatdata <- terra::classify(x,reclass_df, right=FALSE)
  reclassatdata <- terra::classify(reclassatdata,reclass_df2, right=TRUE)
  # Number of presence
  prensce_data <- terra::app(reclassatdata,sum)
  rasUniq <- terra::unique(prensce_data)
  #Number of changes
  change_data <- subset(reclassatdata, 2:nlyr(reclassatdata)) - subset(reclassatdata, 1:(nlyr(reclassatdata)-1))
  change_data <- abs(change_data)
  change_data <- terra::app(change_data,sum)
  rasUniq2 <- terra::unique(change_data)
  return(list("Data for number of presence" = prensce_data,
              "Data for unique number of presence" = rasUniq,
              "Data for number of changes" = change_data,
              "Data for unique number of changes" = rasUniq2))
}
