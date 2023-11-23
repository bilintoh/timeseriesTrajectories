#' Creates results that serve as input for the stackbarPlot function.
#' @param x is the data, which must be a SpatRaste. Default is "unified"
#' @param spatialextent is can be "unified", or any integer defined by the user. If "unified," the change is a percentage of unified region,
#' if user defined integer, the results are in the user’s desired units.
#' unified area; else, the change is a percentage of the  entire region under consideration.
#' @param zeroabsence is a string of "yes" or "no" indicating whether 0 indicates absence or not. default is "yes" meaning 0 means absence.
#' @param timePoints is a vector containing the time points under consideration.The default is  c(2000, 2001, 2002, 2003, 2005).
#' @param annualchange is a string of "yes" or "no". If "yes," results are expressed in annual change. Else, results are expressed in
#' the duration of the time interval. Default is "no"
#' @param categoryName is a character representing the name of the category of interest. Default is "category"
#' @param regionName is a string or character the name of the study region. Default is "region"
#' @import dplyr
#' @import terra
#' @import progress
#' @import reshape2
#' @importFrom stats var
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return The output from \code{\link{rasterstackData}}
#' @import dplyr
#' @import terra
#' @import progress
#' @import reshape2
#' @importFrom stats var
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return The output from \code{\link{rasterstackData}}
#' @export
rasterstackData <- function(x,
                            timePoints = c(2000,2001,2002,2003,2005),
                            spatialextent = 'unified',
                            zeroabsence = 'yes',
                            annualchange = 'yes',
                            categoryName = 'variable',
                            regionName = 'region'){


  j <- blocks(x)
  pb <- txtProgressBar(min = 0, max = j$n, initial = 0, width = 50, style = 3)

  init <- numeric(j$n)
  end <- numeric(j$n)

  if (!class(x) %in% c("SpatRaster")){
    stop("This function is intended for SpatRasters only", call. = FALSE)
  }
  if (!zeroabsence %in% c('no','yes')){
    stop("zeroabsence must have a yes or no input", call. = FALSE)
  }
  if (!annualchange %in% c('no','yes')){
    stop("annualchange must have a yes or no input", call. = FALSE)
  }

  d_gains <- vector('list', j$n) # for gains
  d_loss <- vector('list', j$n) # for losses
  lengthSpext <- vector('list', j$n) # for size of region
  sumLastFirst <- vector('list', j$n)
  d2 <- vector('list', j$n)
  sumLastFirst_2 <- vector('list', j$n)

  ncl_noxy <- nlyr(x)
  m <- ncl_noxy + 1
  k <- ncl_noxy + 2
  v <- ncl_noxy - 1

  clone2 <- data.frame(matrix(1, nrow = 1,ncol = length(1:ncl_noxy)))/j$n

  for (i in seq_along(j$row)) {
    terra::readStart(x)

    d <- na.omit(terra::readValues(x, row = j$row[i], nrows = j$nrows[i], dataframe = T))

    newdf <- d
    newdf$max <- apply(newdf, 1, max, na.rm = TRUE)

    d2 <- sum(abs(d[ncl_noxy] - d[1]))
    sumLastFirst[[i]] <- d2

    d3<- abs(sum(d[ncl_noxy] - d[1]))
    sumLastFirst_2[[i]] <- d3

    if(spatialextent == "unified" & zeroabsence == "yes" & annualchange == 'no'){
      lengthSpext[[i]] <- sum(unlist(newdf$max))
      stackTitle <- paste("Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Change (% of region)"
    } else if(spatialextent == 1 & zeroabsence == "yes" & annualchange == 'no'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Change in",categoryName,"category where extent is",regionName)
    } else if(!spatialextent %in% c('unified', 1) & zeroabsence == "yes" & annualchange == 'no'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Change in",categoryName,"category where extent is",regionName)
      yaxislable <- "Change (% of region)"
    } else if(spatialextent == "unified" & zeroabsence == "no" & annualchange == 'no'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Change (% of region)"
    }else if(spatialextent == 1 & zeroabsence == "no" & annualchange == 'no'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Change in",categoryName,"category where extent is",regionName)
      yaxislable <- "Change (% of region)"
    }else if(!spatialextent %in% c('unified', 1) & zeroabsence == "no" & annualchange == 'no'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Change (% of region)"
    }else if(spatialextent == "unified" & zeroabsence == "no" & annualchange == 'yes'){
      lengthSpext[[i]] <- sum(unlist(newdf$max))
      stackTitle <- paste("Annual Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Annual Change (% of region)"
    }else if(spatialextent == 1 & zeroabsence == "no" & annualchange == 'yes'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Annual Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Annual Change (% of region)"
    }else if(!spatialextent %in% c('unified', 1) & zeroabsence == "no" & annualchange == 'yes'){
      lengthSpext[[i]] <- nrow(clone2)/j$n
      stackTitle <- paste("Annual Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Annual Change (% of region)"
    }else if(spatialextent == "unified" & zeroabsence == "yes" & annualchange == 'yes'){
      lengthSpext[[i]] <- sum(unlist(newdf$max))
      stackTitle <- paste("Annual Change in presence of",categoryName,"category where extent is",regionName)
      yaxislable <- "Annual Change (% of region)"
    }

    input2 <- d[-1] - d[-ncol(d)]

    input2 <- cbind(input2,d[,1],d[,ncl_noxy])

    input2 <-  input2[!apply(input2[,1:v] == 0, 1, all),]


    input2 <- mutate(input2,
                     pos_index = apply(input2[,1:v] > 0, 1, which.max), # positive index
                     neg_index = apply(input2[,1:v] < 0, 1, which.max),
                     pos = rowSums(input2[,1:v] > 0),
                     neg = rowSums(input2[,1:v] < 0),
                     pos_uni = apply(input2[,1:v], 1, function(x) sum(unique(na.omit(x)) > 0)),
                     neg_uni = apply(input2[,1:v], 1, function(x) sum(unique(na.omit(x)) < 0))) #negativ index
    #Create a dummy data
    clone <- data.frame(matrix(0, nrow = 1,ncol = length(input2[,1:v])))
    names(clone) <- names(input2[,1:v])

    #Tajectry type 1
    traj<-input2 %>% subset(input2[ncl_noxy] >  input2[,m] &
                              pos == 0 &
                              neg >= 1)
    if (nrow(traj) == 0){
      seg1GainRed <- as.data.frame(colSums(clone))
      seg1LossRed <- as.data.frame(colSums(clone))
    }else{
      traj <- t(na.omit(traj[,1:v]))
      seg1GainRed <- as.data.frame(apply(traj,1,function(x) sum(x[which(x>0)])))
      seg1LossRed <- as.data.frame(apply(traj,1,function(x) sum(x[which(x<0)])))
    }

    #Tajectry type 2
    traj2<-input2 %>% subset(input2[ncl_noxy] > input2[,m] &
                               #pos_index < neg_index &
                               pos >= 1 &
                               neg  >= 1)

    if (nrow(traj2) == 0){
      seg2GainRed2 <- as.data.frame(colSums(clone))
      seg2LossRed2 <- as.data.frame(colSums(clone))
    }else{
      traj2 <- t(na.omit(traj2[,1:v]))
      seg2GainRed2 <- as.data.frame(apply(traj2,1,function(x) sum(x[which(x>0)])))
      seg2LossRed2 <- as.data.frame(apply(traj2,1,function(x) sum(x[which(x<0)])))

    }


    #Tajectry type 3
    traj3 <- input2 %>% subset(pos >= 1 &
                                 neg == 0)
    if (nrow(traj3) == 0){
      seg3GainBlue <- as.data.frame(colSums(clone))
      seg3LossBlue <- as.data.frame(colSums(clone))
    }else{
      traj3 <- t(na.omit(traj3[,1:v]))
      seg3GainBlue <- as.data.frame(apply(traj3,1,function(x) sum(x[which(x>0)])))
      seg3LossBlue <- as.data.frame(apply(traj3,1,function(x) sum(x[which(x<0)])))
    }

    #Tajectry type 4
    traj4 <- input2 %>% subset(input2[ncl_noxy] < input2[,m] &
                                 #pos_index < neg_index &
                                 pos >= 1 &
                                 neg  >= 1)
    if (nrow(traj4) == 0){
      seg4GainBlue2 <- as.data.frame(colSums(clone))
      seg4LossBlue2 <- as.data.frame(colSums(clone))
    }else{
      traj4 <- t(na.omit(traj4[1:v]))
      seg4GainBlue2 <- as.data.frame(apply(traj4,1,function(x) sum(x[which(x>0)])))
      seg4LossBlue2 <- as.data.frame(apply(traj4,1,function(x) sum(x[which(x<0)])))
      #traj4 <- as.data.frame(colSums(traj4[,1:l] != 0 ))
    }

    #Tajectry type 5:
    traj5 <- input2 %>% subset(input2[,ncl_noxy]==input2[,m] &
                                 rowSums(input2[,1:v]) == 0 &
                                 pos_index > neg_index)
    if (nrow(traj5) == 0){
      traj5 <- clone
      seg5GainBrown <- as.data.frame(colSums(clone))
      seg5LossBrown <- as.data.frame(colSums(clone))
    }else{
      traj5 <- t(na.omit(traj5[,1:v]))
      seg5GainBrown <- as.data.frame(apply(traj5,1,function(x) sum(x[which(x>0)])))
      seg5LossBrown <- as.data.frame(apply(traj5,1,function(x) sum(x[which(x<0)])))
    }

    #Tajectry type 6
    traj6 <- input2 %>% subset(input2[,ncl_noxy] ==  input2[,m] &
                                 rowSums(input2[,1:v]) == 0 &
                                 pos_index < neg_index)
    if (nrow(traj6) == 0){
      seg6GainBrown <- as.data.frame(colSums(clone))
      seg6LossBrown <- as.data.frame(colSums(clone))
    }else{
      traj6 <- t(na.omit(traj6[,1:v]))
      seg6GainBrown <- as.data.frame(apply(traj6,1,function(x) sum(x[which(x>0)])))
      seg6LossBrown <- as.data.frame(apply(traj6,1,function(x) sum(x[which(x<0)])))
    }

    inter_vals <- names(traj)
    gains <- c("gainYellow","gainYellow2",
               "gainLblue","gainLred",
               "gainDblue","gainDred")

    #Loss
    lossess <- c("lossYellow","lossYellow2",
                 "lossLblue","lossLred",
                 "lossDblue","lossDred")
    ls <- list(c("gain","loss"))

    gainDf <-as.data.frame(c(seg5GainBrown,seg6GainBrown,
                             seg4GainBlue2,seg2GainRed2,seg3GainBlue,
                             seg1GainRed))

    oldnames <- names(gainDf)
    newnames <- gains
    gainDf2 <- gainDf %>% rename_at(vars(all_of(oldnames)), ~ newnames)
    trans <- data.frame(cbind(timePoints[-length(timePoints)], timePoints[-1]))
    timeIntervals<- trans[-1] - trans[-ncol(trans)]
    trans$transitions <- paste(trans$X1,"-",trans$X2, sep = "")
    transitions <- trans$transitions
    d_gains[[i]] <- cbind(timeIntervals,gainDf2)


    lossDf <-abs(as.data.frame(c(seg5LossBrown,seg6LossBrown,
                                 seg4LossBlue2, seg2LossRed2,seg3LossBlue,
                                 seg1LossRed)))
    oldnamesLoss <- names(lossDf)
    newnamesLoss <- lossess
    lossDf2 <- lossDf %>% rename_at(vars(all_of(oldnamesLoss)), ~ newnamesLoss)
    d_loss[[i]] <- cbind(timeIntervals,lossDf2)

    Sys.sleep(1)
    setTxtProgressBar(pb, i)
    terra::readStop(x)
  }
  d_loss <- Reduce('+', d_loss )
  d_loss$X2 <- d_loss$X2 / j$n

  d_gains <- Reduce('+', d_gains)
  d_gains$X2 <- d_gains$X2 / j$n

  if(!spatialextent %in% c('unified', 1)){
    lengthSpext <- sum(unlist(lengthSpext)) * spatialextent
  }else {
    lengthSpext <- sum(unlist(lengthSpext))
  }

  sumLastFirst <- Reduce('+', sumLastFirst)
  sumLastFirst_2 <- Reduce('+', sumLastFirst_2)
  if (annualchange == 'no'){
    t_extent <- 1
    gainStack <- (d_gains[-1]/(t_extent * lengthSpext)) * 100
    gainStack$timeIntervals <- d_gains$X2

    lossStack<- ((d_loss[-1] * -1)/(t_extent * lengthSpext)) * 100
    lossStack$timeIntervals <- d_loss$X2

  }else{
    t_extent <- timePoints[ncl_noxy] - timePoints[1]

    gainStack <- (d_gains[-1]/(d_gains$X2 * lengthSpext)) * 100
    gainStack$timeIntervals <- d_gains$X2

    lossStack<- (d_loss[-1]*-1/(d_loss$X2 * lengthSpext)) * 100
    lossStack$timeIntervals <- d_loss$X2
  }

  trajNames <- c( "All Alternation Loss First",
                  "All Alternation Gain First",
                  "Gain with Alternation",
                  "Loss with Alternation",
                  "Gain without Alternation",
                  "Loss without Alternation",
                  "Time_intervals")
  oldnames <- names(gainStack)
  newnames <- trajNames
  gainStack2 <- gainStack %>% rename_at(vars(oldnames), ~ newnames)
  gainStack2b <- t( gainStack2[0:6])
  oldnames <- names(lossStack)
  newnames <- trajNames
  lossStack2 <- lossStack %>% rename_at(vars(oldnames), ~ newnames)
  lossStack2b <- t(lossStack2[0:6])
  lossgainStacked2b <- rbind(gainStack2b,lossStack2b)
  names(lossgainStacked2b) <- transitions
  mergLossGain <- rbind(gainStack2,lossStack2)
  trajOnly <- mergLossGain[1:6]
  maxGain <- max(trajOnly[trajOnly > 0])
  maxLoss <- max(abs(trajOnly[trajOnly < 0]))
  mergLossGain$interval_2 <- transitions
  mergLossGain3 <- mergLossGain[ , c("Time_intervals",
                                     names(mergLossGain)[names(mergLossGain) != "Time_intervals"])]
  mergLossGain4 <- mergLossGain3[1:7]
  transLossGain4 <- t(mergLossGain4)
  transLossGain5 <- transLossGain4[- 1,]
  colnames(transLossGain5) <- c(mergLossGain$interval_2)
  meltLossGain5 <- reshape2::melt(transLossGain5)
  colnames(transLossGain5) <- c(mergLossGain$Time_intervals)
  meltLossGain5b <- reshape2::melt(transLossGain5)
  meltLossGain5$size <- meltLossGain5b$Var2
  prodGainLossInt <- meltLossGain5$value*meltLossGain5$size

  gainLine <- sum(prodGainLossInt[prodGainLossInt > 0])/(timePoints[ncl_noxy] - timePoints[1])
  lossLine <- sum(prodGainLossInt[prodGainLossInt < 0])/(timePoints[ncl_noxy] - timePoints[1])
  net <- (gainLine + lossLine)

  if (net < 0){
    Net <- "Quantity Loss"
  } else if (net > 0){
    Net <- "Quantity Gain"
  } else{
    Net <- "Zero Quantity"
  }
  netAbs <- abs(net)

  allocation <- (sumLastFirst * (100/ lengthSpext)) / (timePoints[ncl_noxy] - timePoints[1]) - netAbs
  alternation <- gainLine - lossLine - allocation - netAbs
  nameRegion <- c("Annual Change in region Y",
                  "Annual Change in region Y2",
                  "Annual Change in region Y3")
  compNames <- c(Net,"Allocation","Alternation")
  compVals <- c(netAbs,allocation,alternation)
  dfCompnents <- as.data.frame(compVals,nameRegion)
  dfCompnents$compNames <- compNames
  dfCompnents2 <- reshape2::melt(dfCompnents, id = "compNames")
  trajNames2 <- c( "All Alternation Loss First",
                   "All Alternation Gain First",
                   "Gain with Alternation",
                   "Loss with Alternation",
                   "Gain without Alternation",
                   "Loss without Alternation")
  trajCol <- c('#a8a803','#e6e600','#14a5e3',
                        '#ff6666','#020e7a','#941004')
                        nameCol1 <- as.data.frame(cbind(trajNames2,trajCol))
                        trajNames3 <- colSums(abs(mergLossGain[1:6]))
                        trajNames3 <- as.data.frame(names(trajNames3[trajNames3!=0]))
                        names(trajNames3) <- "trajNames2"
                        nameCol2 <- left_join(trajNames3,nameCol1,by = "trajNames2")

  close(pb)
  return(list("Factor dataframe for trajectory stacke bar plot" = meltLossGain5,
              "Value of gain line" = gainLine,
              "Value of loss line" = lossLine,
              "Dataframe for components of change" = dfCompnents2,
              "Title of stackbar plot" = stackTitle,
              "Size of net component" = Net,
              "Name of category of ineterst" = categoryName,
              "Dataframe for stackbar plot" = mergLossGain,
              "Colors and trajectories for stacked bars" = nameCol2,
              "vertical axis labe" = yaxislable,
              sumLastFirst,
              sumLastFirst_2,
              lengthSpext))
}
