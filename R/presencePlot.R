#' Creates a map and pie chart of the trajectories of the category of interest.
#' @param input is the results from running the "presenceData" function.
#' @param pltunit is the unit in which the current map is plotted in, one of cm, m, km, in, ft, mi. or lat or lon.
#' @param dataEpsg is the projection of the current map. If extents are valid lat and lons,
#' the projection is assumed to be lat and lon (EPSG:4326), or Spherical Mercator otherwise.
#' @param categoryName is a character representing the name of the category of interest.Default is "marsh".
#' @param scalePos where to align the scale bar. One of "bottomleft", "bottomright", "topleft", or "topright".
#' @param narrowPos is a numeric value indicating the size of the north arrow.
#' @param narrowSize is a numeric value indicating the size of the north arrow.
#' @param xAxis is a character indicating label for the horizontal axis. default is "Longitude (m)".
#' @param yAxis is a character indicating label for the vertical axis. default is "Latitude (m)".
#' @param axisText is a numeric value controlling the size of the text on the horizontal and vertical ticks.
#' @param axisLabel is a numeric value controlling the size of the  horizontal and vertical labels
#' @param plotTitle is a numeric value controlling the size of the plot title.
#' @import prettymapr
#' @import RColorBrewer
#' @importFrom grDevices colorRampPalette
#' @return The output from \code{\link{presencePlot}}
#' @export
presencePlot <- function(input,
                         pltunit = "m",
                         dataEpsg = 4326,
                         scalePos = "bottomright",
                         narrowPos = "topleft",
                         narrowSize = 1,
                         categoryName = "marsh",
                         xAxis = "Longitude (m)",
                         yAxis = "Latitude (m)",
                         axisText = 1.1,
                         axisLabel = 1.2,
                         plotTitle = 1.5){
  op <- graphics::par(mgp=c(1.5,1,0))
  on.exit(graphics::par(op))

  n1 <- nrow(input[[2]]) + 1
  n2 <- length(table(input[[4]])) + 1

  cc <- c("#c4c3c0",colorRampPalette(brewer.pal(9, "Oranges"))(n1))
  cc <- cc[cc!="#FFFFFF"]

  cc2 <- c("#c4c3c0",colorRampPalette(brewer.pal(9, "Greens"))(n2))
  cc2 <- cc2[cc2!="#FFFFFF"]

  terra::plot(input[[1]],
              col = cc,
              colNA = "White",
              main = paste("Number of times",categoryName,"is present during the temporal extent. White is NA"),
              xlab = xAxis,
              ylab = yAxis,
              pax=list( # parameters for drawing axes
                cex.axis = axisText), # Axis text size ,
              cex.lab = axisLabel,
              cex.main = plotTitle)
  prettymapr::addscalebar(pltunit,
                          plotepsg  = dataEpsg,
                          pos = scalePos)
  prettymapr::addnortharrow(pos = narrowPos,
                            scale = narrowSize)

  terra::plot(input[[3]],
              col = cc2,
              colNA = "White",
              main = paste("Number of times",categoryName,"changes during the temporal extent. White is NA"),
              xlab = xAxis,
              ylab = yAxis,
              pax=list( # parameters for drawing axes
                cex.axis = axisText), # Axis text size ,
              cex.lab = axisLabel,
              cex.main = plotTitle)
  prettymapr::addscalebar(pltunit,
                          plotepsg  = dataEpsg,
                          pos = scalePos)
  prettymapr::addnortharrow(pos = narrowPos,
                            scale = narrowSize)
}
