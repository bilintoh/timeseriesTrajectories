#' creates the data which serves as input for the "trajPlot" function.
#' @param x is the raster stack.
#' @param zeroabsence is a string of "yes" or "no" indicating if 0 means absence. # The unified size is the union of the locations where the category exists at any time point. The default is "yes".
#' @param unified is a string of "yes" or "no" indicating if the analysis involves the unified size. The default is "yes"
#' @param user_spatial is an integer for the user-defined spatial extent. Default is 0.
#' @import dplyr
#' @import terra
#' @import progress
#' @import sf
#' @importFrom stats var
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom data.table rbindlist
#' @return The output from \code{\link{rastertrajData}}
#' @export
rastertrajData <- function(x,
                            zeroabsence = 'yes',
                            unified = "yes",
                            user_spatial = 0){

  rastcrs <- crs(x)
  spatialres <- res(x)
  rastextent <- ext(x)


  if (!class(x) %in% c("SpatRaster")){
    stop("This function is intended for SpatRasters only", call. = FALSE)
  }


  if (!zeroabsence %in% c('no','yes')){
    stop("zeroabsence must have a yes or no input", call. = FALSE)}

  j <- blocks(x)
  pb <- txtProgressBar(min = 0, max = j$n, initial = 0, width = 50, style = 3)



  dimpixels <- terra::nrow(x[[1]]) * terra::ncol(x[[1]])
  numpix_a <- j$nrows[1] * ncol(x[[1]])

  # To
  f1 <- c(1:(j$n - 1))
  f2 <- f1 * numpix_a
  f3 <- c(f2,dimpixels)

  #from
  z <- f2 + 1
  z2 <- c(1,z)

  #nCl <- length(d)# number of columns
  ncl_noxy <- nlyr(x)
  m <- ncl_noxy + 1
  k <- ncl_noxy + 2
  s <- ncl_noxy + 3
  v <- ncl_noxy - 1

  #dfxyz <- vector('list', j$n)
  unified_size <- vector('list', j$n)
  lst_trajdf <- vector('list', j$n)
  clone1 <- data.frame(matrix(0, nrow = 1,ncol = 3))
  names(clone1) <- c("x","y","change")
  clone1$change <- NA

  terra::readStart(x)
  for (i in seq_along(j$row)) {

    df <- terra::readValues(x, row = j$row[i], nrows = j$nrows[i], dataframe=T)

    if (j$n == 1){
      dfxy <- terra::xyFromCell(x, c(1:dimpixels))

    }else if(j$n > 1){
      dfxy <- terra::xyFromCell(x, c(z2[i]:f3[i]))
    }

    max_size <- sum(apply(na.omit(df), 1, max, na.rm=TRUE))

    d <- cbind(df,dfxy)

    d <- na.omit(d)


    traj1 <- d[apply(d[,1:ncl_noxy], 1, var) == 0, ]
    if (nrow(traj1) == 0){
      traj1 <- clone1
    }else if (nrow(traj1) > 0) {
      if (zeroabsence %in% c('no')){
        traj1 <- traj1
        traj1 <- mutate(traj1[m:k],change = 7)
      } else if (zeroabsence %in% c('yes')){
        traj1 <- traj1 %>% filter_at(vars(1:ncl_noxy), all_vars(. > 0))
        traj1 <- mutate(traj1[m:k],change = 7)
      }
    }
    traj2 <- d %>% filter_at(vars(1:ncl_noxy), all_vars(. == 0))
    if (nrow(traj2) == 0){
      traj1 <- clone1
    }else if (nrow(traj2) > 0) {
      if (zeroabsence %in% c('no')){
        traj2 <- clone1
      } else if (zeroabsence %in% c('yes')){
        traj2 <- d %>% filter_at(vars(1:ncl_noxy), all_vars(. == 0))
        traj2 <- mutate(traj2[m:k],change = 8)
      }
    }


    if (ncl_noxy == 2){
      input2 <- cbind(d[1:as.numeric(ncl_noxy)][-1] -
                        d[1:as.numeric(ncl_noxy)]
                      [-ncol(d[1:as.numeric(ncl_noxy)])],
                      d[m:k])

      input2 <- input2[input2[,1:1] != 0, ]

      #Tajectry type 1
      traj3<-input2 %>% subset(input2[1] < 0 )
      if (nrow(traj3) == 0){
        traj3 <- clone1
      }else{
        traj3 <- mutate(traj3[2:3],change = 1)
      }

      #Traj4
      traj4 <- clone1

      #Tajectry type 3
      traj5 <- input2 %>% subset(input2[1] > 0)

      if (nrow(traj5) == 0){
        traj5 <- clone1
      }else{
        traj5 <- mutate(traj5[2:3],change = 3)
      }

      #trajectory4
      traj6 <- clone1

      #trajectory5
      traj7 <- clone1

      #trajectory6
      traj8 <- clone1

    }else{

      input2 <- cbind(d[1:as.numeric(ncl_noxy)][-1] -
                        d[1:as.numeric(ncl_noxy)]
                      [-ncol(d[1:as.numeric(ncl_noxy)])],
                      d[,1],
                      d[,ncl_noxy],
                      d[m:k])

      input2 <- input2[!apply(input2[,1:v], 1, var) == 0, ]


      input2 <- mutate(input2,
                       pos_index = apply(input2[,1:v] > 0, 1, which.max), # positive index
                       neg_index = apply(input2[,1:v] < 0, 1, which.max),
                       pos = rowSums(input2[,1:v] > 0),
                       neg = rowSums(input2[,1:v] < 0),
                       pos_uni = apply(input2[,1:v], 1, function(x) sum(unique(na.omit(x)) > 0)),
                       neg_uni = apply(input2[,1:v], 1, function(x) sum(unique(na.omit(x)) < 0)))

      #Tajectry type 1
      traj3<-input2 %>% subset(input2[ncl_noxy] >  input2[,m] &
                                 pos == 0 &
                                 neg >= 1)
      if (nrow(traj3) == 0){
        traj3 <- clone1
      }else{
        traj3 <- mutate(traj3[k:s],change = 1)
      }

      #Tajectry type 2
      traj4 <- input2 %>% subset(input2[ncl_noxy] > input2[,m] &
                                   pos >= 1 &
                                   neg  >= 1)
      if (nrow(traj4) == 0){
        traj4 <- clone1
      }else{
        traj4 <- mutate(traj4[k:s],change = 2)
      }

      #Tajectry type 3
      traj5 <- input2 %>% subset(pos >= 1 &
                                   neg == 0)

      if (nrow(traj5) == 0){
        traj5 <- clone1
      }else{
        traj5 <- mutate(traj5[k:s],change = 3)
      }

      #Tajectry type 4
      traj6 <- input2 %>% subset(input2[ncl_noxy] < input2[,m] &
                                   pos >= 1 &
                                   neg  >= 1 )
      tx <- traj6
      if (nrow(traj6) == 0){
        traj6 <- clone1
      }else{
        traj6 <- mutate(traj6[k:s],change = 4)
      }

      #Tajectry type 5: All Alternation Loss First	Dark Yellow
      traj7 <- input2 %>% subset(input2[,ncl_noxy]==input2[,m] &
                                   rowSums(input2[,1:v]) == 0 &
                                   pos_index > neg_index)
      if (nrow(traj7) == 0){
        traj7 <- clone1
      }else{
        traj7 <- mutate(traj7[k:s],change = 5)
      }

      #trajectory type 6 : All Alternation Gain First	Light Yellow
      traj8 <- input2 %>% subset(input2[,ncl_noxy] ==  input2[,m] &
                                   rowSums(input2[,1:v]) == 0 &
                                   pos_index < neg_index)
      if (nrow(traj8) == 0){
        traj8 <- clone1
      }else{
        traj8 <- mutate(traj8[k:s],change = 6)
      }


    }

    unified_size[[i]] <- max_size

    trajdf <-  na.omit(rbind(traj1,traj2,traj3,traj4,traj5,traj6,traj7,traj8))
    lst_trajdf[[i]] <- trajdf

    Sys.sleep(1)
    setTxtProgressBar(pb, i)
  }


  unified_size2 <- Reduce("+", unified_size)

  lst_trajdf <- rbindlist(lst_trajdf)

  if(nrow(lst_trajdf) > 2){
    dfr <- terra::rast(lst_trajdf,type = "xyz",crs = rastcrs)

  }else if (nrow(lst_trajdf) < 3){
    dummy_rast <- x[[1]]
    dfr <- setValues(dummy_rast, lst_trajdf$change)
  }

  #lst_trajdf <- trajdf

  dfr2 <- terra::as.factor(dfr)
  rat <- terra::levels(dfr2)[[1]]
  rat <- na.omit(terra::as.data.frame(rat))

  #Pie Chart information
  pixCount<- table(unlist(lst_trajdf$change))

  #rbind(q, c(8,'#666666',"Stable Absence", 1))

  if(unified == "yes" & zeroabsence == "yes" & user_spatial == 0){

    colnames(rat) <- "ID"
    cl <-
      c(
        "Loss without Alternation",
        "Loss with Alternation",
        "Gain without Alternation",
        "Gain with Alternation",
        "All Alternation Loss First",
        "All Alternation Gain First",
        "Stable Presence",
        "Stable Absence"
      )
    myCol = c('#941004','#FF6666','#020e7a','#14a5e3','#a8a803','#E6E600','#666666','#c4c3c0')
    cl2 <-
      c(
        "Loss without Alternation",
        "Loss with Alternation",
        "Gain without Alternation",
        "Gain with Alternation",
        "All Alternation Loss First",
        "All Alternation Gain First",
        "Stable Presence",
        "Stable Absence"
      )
    myCol2 = c('#941004','#FF6666','#a8a803','#E6E600','#14a5e3','#020e7a','#666666','#c4c3c0')
    ID <- c(1,2,3,4,5,6,7,8)
    idmyColcl <- data.frame(ID,myCol,cl)
    df3 <- left_join(rat,idmyColcl , by = "ID")
    dfPie <- df3
    df3 <- df3[,-2]
    dfpie <- cbind(dfPie,pixCount)
    dfPie[["value"]] <- pixCount
    dfPie2 <- dfPie[,-2] %>%slice(match(cl2, cl))
    dfPie2 <- subset(dfPie2, ID!= 8)

    #stackTitle <- paste("Change in presence of",categoryName,"category where extent is",regionName)
  } else if(unified == "yes" & zeroabsence == "yes" & user_spatial != 0){

    colnames(rat) <- "ID"
    cl <-
      c(
        "Loss without Alternation",
        "Loss with Alternation",
        "Gain without Alternation",
        "Gain with Alternation",
        "All Alternation Loss First",
        "All Alternation Gain First",
        "Stable Presence",
        "Stable Absence"

      )
    myCol = c('#941004','#FF6666','#020e7a','#14a5e3','#a8a803','#E6E600','#666666','#c4c3c0')
    cl2 <-
      c(
        "Loss without Alternation",
        "Loss with Alternation",
        "Gain without Alternation",
        "Gain with Alternation",
        "All Alternation Loss First",
        "All Alternation Gain First",
        "Stable Presence"

      )
    myCol2 = c('#941004','#FF6666','#a8a803','#E6E600','#14a5e3','#020e7a','#666666')
    ID <- c(1,2,3,4,5,6,7,8)
    idmyColcl <- data.frame(ID,myCol,cl)
    df3 <- left_join(rat,idmyColcl , by = "ID")
    dfPie <- df3
    df3 <- df3[,-2]
    dfpie <- cbind(dfPie,pixCount)
    dfPie[["value"]] <- pixCount
    dfPie2 <- dfPie[,-2] %>%slice(match(cl2, cl))
    diff_spatial <- user_spatial - sum(dfPie2$value)
    dfPie2 <- rbind(dfPie2, c(8, '#c4c3c0', "Stable Absence", diff_spatial))

    #stackTitle <- paste("Change in presence of",categoryName,"category where extent is",regionName)
  } else {
    colnames(rat) <- "ID"
    cl <-
      c(
        "Loss without Alternation",
        "Loss with Alternation",
        "Gain without Alternation",
        "Gain with Alternation",
        "All Alternation Loss First",
        "All Alternation Gain First",
        "Stable"
      )
    myCol = c('#941004','#FF6666','#020e7a','#14a5e3','#a8a803','#E6E600','#666666')
    cl2 <-
      c(
        "Loss without Alternation",
        "Loss with Alternation",
        "Gain without Alternation",
        "Gain with Alternation",
        "All Alternation Loss First",
        "All Alternation Gain First",
        "Stable"
      )
    myCol2 = c('#941004','#FF6666','#a8a803','#E6E600','#14a5e3','#020e7a','#666666')
    ID <- c(1,2,3,4,5,6,7)
    idmyColcl <- data.frame(ID,myCol,cl)
    df3 <- left_join(rat,idmyColcl , by = "ID")
    df3 <- df3[,-2]
    dfPie <- df3
    dfpie <- cbind(dfPie,pixCount)
    dfPie[["value"]] <- pixCount
    dfPie2 <- dfPie[,-2] %>%slice(match(cl2, cl))

  }
  close(pb)
  return(list("Raster data for trajectory plot" = dfr,
              "Attribute data for trajectory plot" = df3,
              "Data for trajectory pie chart" = dfPie2,
              "Number of time points" = ncl_noxy))
}
