#' Create stack bar plots showing gross losses and gains of the trajectories and the three change components.
#' @param input is the results from running the "dataStack" function.
#' @param axisSize is a numerical value that control the size of the labels on tick marks of the horizontal
#' and vertical tick marks.
#' @param lbAxSize is a numerical value to control the size of the labels on the horizontal and vertical axis.
#' @param lgSize is a numerical value to control the size of the legend text.
#' @param titleSize is a numerical value to control the size of the title text.
#' @param datbreaks is a string of "yes" or "no", which controls the range and sub-division of the vertical axis
#' of the stacked bar plots.The default is "no", which automatically generates the range and interval of the vertical axis.
#' If "no" the user need to mannual input values for "upperlym","lowerlym", "lymby","upperlym2", and "lymby2".
#' @param upperlym if datbreaks set to "yes," is a numerical value to control the upper limit of the trajectory stack bar plot.
#' @param lowerlym if datbreaks set to "yes," is a numerical value to control the lower limit of the trajectory stack bar plot.
#' @param lymby if datbreaks set to "yes," is a numerical value to control interval on the vertical axis of the components of change stack bar plot.
#' @param upperlym2 if datbreaks set to "yes," is a numerical value to control the upper limit of the components of change stacked bar plot.
#' @param lymby2 if datbreaks set to "yes," is a numerical value to control the interval on the vertical axis of the components of change stacked bar plot.
#' @param xAngle  is a numerical value to control the orientation of the text on the vertical axis of the trajectory stack bar plot
#  One of 0,90,180 or 360.
#' @return The output from \code{\link{stackbarPlot}}
#' @import ggplot2
#' @import ggnewscale
#' @export
stackbarPlot <- function(input,
                          axisSize = 12,
                          lbAxSize = 15,
                          lgSize = 12,
                          titleSize = 15,
                          datbreaks = "no",
                          upperlym = 35,
                          lowerlym = - 50,
                          lymby = 5,
                          upperlym2 = 0.5,
                          lymby2 = 0.1,
                          xAngle = 0){
  if (!datbreaks %in% c("yes","no")){
    stop("The input must be'yes' or 'no'",call. = FALSE)
  } else if (datbreaks %in% ("yes")) {
    v1 <- scale_y_continuous(breaks = seq(lowerlym,upperlym, by = lymby),
                             limits=c(lowerlym,upperlym))
    v2 <- scale_y_continuous(limits = c(0, upperlym2),
                             breaks = seq(0, upperlym2, by = lymby2),
                             expand = c(0,0))
  } else if (datbreaks %in% ("no")){
    v1 <- scale_y_continuous()
    v2 <- scale_y_continuous(expand = c(0,0))
  }
  a <- ggplot(input[[1]], aes(factor(input[[1]]$Var2),input[[1]]$value,
                              fill = factor(input[[1]]$Var1,
                                            levels = factor(input[[9]]$trajNames2)),
                              width = input[[1]]$size)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = as.character(input[[9]]$trajCol),na.translate = F)+
    v1+
    geom_abline(aes(slope=0, intercept=input[[2]], color = "Gross Gain"),key_glyph = "path",lty=4, size = 0.8) +
    scale_colour_manual(name="",values = c("Gross Gain" = "black"))+
    new_scale("color")+
    geom_abline(aes(slope=0, intercept=input[[3]], color = "Gross Loss"),key_glyph = "path",lty=3, size = 0.8)+
    scale_colour_manual(name="",values = c("Gross Loss" = "black"))+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),text = element_text(size = 8)) +
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA))+
    theme(legend.text=element_text(size=rel(1)))+
    scale_x_discrete(expand = c(0, 0))+
    facet_grid(~Var2, scales = "free_x", space = "free_x")+
    guides(fill = guide_legend(title = "")) +
    geom_abline(slope=0, intercept=0,  col = "grey",size = 0.5) +
    xlab("Time Interval") +
    ylab = input[[10]]+
    ggtitle(input[[5]])+
    theme(
      plot.title = element_text(size = titleSize, face = "bold"))+
    theme(
      panel.spacing = unit(0,'lines'),strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.01))+
    theme(axis.ticks.x = element_blank() )+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "white",
                                  fill = NA,
                                  size = NA))+
    theme(axis.line.y = element_line(color = "black",
                                     size = 1)) +
    theme(axis.text=element_text(size=axisSize,face="bold"),
          axis.title=element_text(size=lbAxSize,face="bold"),
          legend.position= 'bottom',
          legend.title=element_text(size=18,face="bold"),
          legend.text = element_text( size = lgSize, face = "bold"),
          legend.spacing.y = unit(-0.2, "lines"),
          legend.margin = margin(0, 0, 0, 0),
          legend.key = element_rect(colour = NA, fill = NA))+
    theme(axis.text.x = element_text(angle = xAngle))
  b <- ggplot(input[[4]], aes(input[[4]]$variable,input[[4]]$value,
                              fill=factor(input[[4]]$compNames,
                                          levels=c("Alternation","Allocation",input[[6]])))) +
    geom_bar(stat = "identity")+
    scale_fill_manual(values = c("#D3D3D3","#A9A9A9",'#808080'))+
    scale_x_discrete(expand = expansion(add=c(0,0)))+
    guides(fill = guide_legend(title = ""))+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),text = element_text(size = 8)) +
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA))+
    theme(axis.text.x=element_blank())+
    xlab("All time intervals") +
    ylab = input[[10]] +
    ggtitle(input[[5]]) +
    theme(
      plot.title = element_text(size = titleSize, face = "bold"))+
    theme(
      panel.spacing = unit(0,'lines'),strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.01))+
    theme(axis.text=element_text(size=axisSize,face="bold"),
          axis.title=element_text(size=15,face="bold"),legend.position= 'right',
          legend.title=element_text(size=18,face="bold"),
          legend.text = element_text( size = 12, face = "bold"))+
    theme(axis.ticks.x = element_blank())+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "white",
                                  fill = NA,
                                  size = NA))+
    theme(axis.line.y = element_line(color = "black",
                                     size = 1))+
    theme(axis.line.x = element_line(color = "black",
                                     size = 1))+
    v2
  return(list(a,b))
}
