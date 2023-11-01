# This function creates the map and pie chart for the trajectories of change.
#' Creates a map and pie chart of the trajectories of the category of interest.
#' @param input is the results from running the "raster_trajdata" function.
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
#' @param legendTex is a numerical value controlling the size of the legend text.
#' @import prettymapr
#' @import RColorBrewer
#' @importFrom classInt classIntervals
#' @importFrom grDevices colorRampPalette
#' @return The output from \code{\link{trajPlot}}
#' @export
trajPlot <- function(input,
                     pltunit = "m",
                     dataEpsg = 32632,
                     categoryName = "marsh's",
                     scalePos = "bottomright",
                     narrowPos = "topleft",
                     narrowSize = 0.6,
                     xAxis = "Longitude (m)",
                     yAxis = "Latitude (m)",
                     axisText = 1.2,
                     axisLabel = 1.4,
                     plotTitle = 1.5,
                     legendTex = 0.9){
  clon <- data.frame(matrix(9, nrow = 1,ncol = 3))
  clon$X2 <- "#ffffff"
    clon$X3 <- "No Data"
    names(clon) <- c("ID","myCol","cl")
    lengData <- input[[2]]
    lengData <- rbind(lengData,clon)
    op1 <- graphics::par(xpd = FALSE)
    on.exit(graphics::par(op1))
    op2 <- graphics::par(mai = c(1, 1, 1, 3) + 0.1)
    on.exit(graphics::par(op2))

    my_data_row <- nrow(input[[1]])
    my_data_col <- ncol(input[[1]])
    df_row <- nrow(input[[2]])

    # I put in the if and else if statements to cater for situations where
    # there one unique pixel values.
    if(df_row > 1 ){
      breaks_qt <- classIntervals(input[[2]]$ID,n = length(input[[2]]$ID), warnSmallN = FALSE )
      terra::plot((input[[1]]),
                  col = as.character(input[[2]]$myCol),
                  breaks = breaks_qt$brks,
                  #col = as.character(input[[2]]$myCol),
                  legend = F,
                  colNA = "White",
                  main = paste(categoryName, "trajectories","for",
                               input[[4]] - 1,"time intervals"),
                  xlab = xAxis,
                  ylab = yAxis,
                  cex.axis =  axisText,
                  cex.lab = axisLabel,
                  cex.main = plotTitle)
      coord <- graphics::par("usr")
      op3 <- graphics::par(xpd = TRUE)
      on.exit(graphics::par(op3))

      prettymapr::addscalebar(pltunit,
                              plotepsg  = dataEpsg,
                              pos = scalePos)
      prettymapr::addnortharrow(pos = narrowPos,
                                scale = narrowSize)

    }else if (df_row == 1 ){
      #breaks_qt <- classIntervals(input[[2]]$ID,n = length(input[[2]]$ID), warnSmallN = FALSE )
      terra::plot((input[[1]]),
                   col = as.character(input[[2]]$myCol),
                   #breaks = breaks_qt$brks,
                   legend = F,
                   colNA = "White",
                   main = paste(categoryName, "trajectories","for",
                                input[[4]],"time points"),
                   xlab = xAxis,
                   ylab = yAxis,
                   cex.axis =  axisText,
                   cex.lab = axisLabel,
                   cex.main = plotTitle)
      coord <- graphics::par("usr")
      op3 <- graphics::par(xpd = TRUE)
      on.exit(graphics::par(op3))

      prettymapr::addscalebar(pltunit,
                              plotepsg  = dataEpsg,
                              pos = scalePos)
      prettymapr::addnortharrow(pos = narrowPos,
                                scale = narrowSize)}

    graphics::pie(as.numeric(input[[3]]$value),
                  clockwise = TRUE,
                  labels = "",
                  col=as.character(input[[3]]$myCol ),
                  main = "Size of trajectories",
                  cex = plotTitle)
    graphics::legend("topright",
                     title = "Trajectories",
                     legend = input[[3]]$cl,
                     fill = as.character(input[[3]]$myCol),
                     border = TRUE,
                     bty = "n",
                     cex = legendTex)
}
