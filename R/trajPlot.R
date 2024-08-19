# This function creates the map for the trajectories of change.
#' Creates a map and pie chart of the trajectories of the category of interest.
#' @param input is the results from running the "raster_trajdata" function.
#' @param axisShow dictates whether the plot axes is shown or not. "no" means
#' no axes, whiles yes means the axes would be shown. The default is "no".
#' @param categoryName is a character representing the name of the category of interest.Default is "marsh".
#' @param scalePos where to align the scale bar. One of "bottomleft", "bottomright", "topleft", or "topright".
#' @param scaleSize is a numeric value indicating the size of the scale bar.
#' @param narrowPos is a numeric value indicating the size of the north arrow.
#' @param xAxis is a character indicating label for the horizontal axis. default is "Longitude (m)".
#' @param yAxis is a character indicating label for the vertical axis. default is "Latitude (m)".
#' @param axisText is a numeric value controlling the size of the text on the horizontal and vertical ticks.
#' @param axisLabel is a numeric value controlling the size of the  horizontal and vertical labels
#' @param plotTitle is a numeric value controlling the size of the plot title.
#' @param legendTex is a numerical value controlling the size of the legend text.
#' @param downsample is a string of "TRUE" or "FALSE" to down sample the spatial resolution of the plot.
#' "TRUE" down samples the plot.
#' @import tmap
#' @import tmaptools
#' @import RColorBrewer
#' @importFrom classInt classIntervals
#' @importFrom grDevices colorRampPalette
#' @return The output from \code{\link{trajPlot}}
#' @export
trajPlot <- function(input,
                     axisShow = "no",
                     categoryName = "forest",
                     narrowPos = NA,
                     scalePos = NA,
                     scaleSize = 0.8,
                     axisText = 1.2,
                     axisLabel = 1.2,
                     plotTitle = 1.3,
                     legendTex = 1,
                     xAxis = "Longitude (m)",
                     yAxis = "Latitude (m)",
                     downsample = TRUE){


  if (axisShow == "no"){
    tm_shape(input[[1]],
             raster.downsample = downsample) +
      tm_raster(style= "cat",
                alpha = 1,
                palette = c(input[[2]]$myCol,"white"),
                labels  = input[[2]]$cl,
                title = "Trajectories") +
      tm_layout(legend.outside = TRUE,
                legend.outside.position = c("right","top"),
                legend.title.size = 1.5,
                inner.margins = 0,
                legend.text.size = legendTex,
                main.title.size = plotTitle,
                main.title =  paste(categoryName,
                                    "trajectories","for",
                                    input[[3]] - 1,
                                    "time intervals.", "White is NA"))+
      tm_compass(position = narrowPos) +
      tm_scale_bar(text.size = scaleSize, position = scalePos)

  }else{
    tm_shape(input[[1]],
             raster.downsample = downsample) +
      tm_grid() +
      tm_raster(style= "cat",
                alpha = 1,
                palette = c(input[[2]]$myCol,"white"),
                labels  = input[[2]]$cl,
                title = "Trajectories") +
      tm_layout(legend.outside = TRUE,
                legend.outside.position = c("right","top"),
                legend.title.size = 1.5,
                inner.margins = 0,
                legend.text.size = legendTex,
                main.title.size = plotTitle,
                main.title =  paste(categoryName,
                                    "trajectories","for",
                                    input[[3]],
                                    "time points.", "White is NA"))+
      tm_xlab(xAxis, size = axisLabel, rotation = 0, space = 0.6)+
      tm_ylab(yAxis, size = axisLabel, rotation = 90, space = 0.8) +
      tm_compass(position = narrowPos) +
      tm_scale_bar(text.size = scaleSize, position = scalePos)
  }

}
