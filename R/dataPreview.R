#' This function will allow the user to create a time series plot of the data.
#' @param x is the data which must be a rasterstack.
#' @param timepoints is a vector of numerical values represneting your time points.
#' @param vertunits is a string that states the units of the y values.
#' @param xAngle is the orientation for the vertical axis labels. The default is 90.
#' @import ggplot2
#' @return The output from \code{\link{dataPreview}}
#' @export
dataPreview <- function(x,
                    timepoints = c(2000,2001,2002,2003,2005),
                    vertunits = "millions of variable",
                    xAngle = 90){

  anualProp <- terra::global(x, "sum", na.rm=TRUE)
  anualProp <- anualProp$sum
  names(anualProp) <- 'anualProp'

  #create a data frame containing the data
  df <- data.frame(anualProp,timepoints)
  maxsize <- max(df[,1])

  # create plot for data
  vertlab <- paste0("Size",'', "(","as",' ', vertunits,")")
  horizlab <- "Time"

  p <- ggplot(data = df,aes(x = timepoints, y = anualProp)) +
    geom_line(aes(timepoints))+
    geom_point()+
    scale_x_continuous(labels=as.character(df[,2]),breaks=df[,2])+
    #scale_y_continuous(expand = c(0,0),limits = c(470815,maxsize + (maxsize/10)))+
    labs(y= vertlab, x = horizlab)+
    ggtitle("Size of variable at each time point")+
    theme(
      plot.title = element_text(size=20, face="bold"))+


    theme(
      panel.background = element_rect(fill = "transparent",colour = NA))+
    theme(legend.text=element_text(size=rel(1)))+
    theme(axis.line.y = element_line(color = "black",size = 0.5),
          axis.line.x = element_line(color = "black",size = 0.5))+

    theme(axis.text.x = element_text(angle = xAngle))+
    theme(axis.text=element_text(size=10,face="bold"),
          axis.title=element_text(size=14,face="bold"),
          legend.position= 'bottom',
          legend.title=element_text(size=18,face="bold"),
          legend.text = element_text( size = 12, face = "bold"))
  #scale_y_continuous(expand = c(0,0))

  return(p)
}
