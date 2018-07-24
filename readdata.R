#This is a script that lets us read in specific preprocess measurement data necessary for calculating respiration rates for a specific timestamp

#install and load necessary packages
require(segmented); require(tibble)

# add the library 'RespChamberProc' package by sourcing 
setwd ('/Users/bdavis/Documents/HF REU/My Project/48HR/source/RespChamberProc/')
fileNames <- list.files (pattern = "*.R") [-c (9:10)]
res <- sapply (fileNames, source); rm (res)

#LOWER AND UPPERBOUNDS
setwd("~/Documents/HF REU/My Project/48HR")
bounds<-read.csv(file = 'SoilResp2018_UpperLower.csv', header = TRUE) 

lowerbound<-bounds[bounds$TIMESTAMP==date_time,seq(2,ncol(bounds),2)]
upperbound<-bounds[bounds$TIMESTAMP==date_time,seq(3,ncol(bounds),2)] #this is multiplied by a factor of 10 b/c the 'RunTime' needed to be *10 sometimes, in order to exceed the 60 second requirment in the resFit function 


#CHAMBER DIMENTIONS (unique to each chamber)

soilH<-read.csv("SoilChamberHeights.csv",header=FALSE) 
stemH<-read.csv("StemChamberHeights.csv",header=FALSE)
names (soilH)<- c("tree",'chamber','h1_cm','h2_cm','h3_cm','h4_cm','havg_cm')
names (stemH)<- c('tree','chamber','h1_cm','h2_cm','h3_cm','h4_cm','havg_cm')
soilH$havg_m <- soilH$havg_cm/100
stemH$havg_m <- stemH$havg_cm/100

soilH$vol_m3 <- calcChamberGeometryCylinder (radius = 0.1016,
                                                  height = soilH$havg_m,
                                                  taper  = 1.0) [-(dim(soilH)[1]+1)] #remove the last value 

soilH$respArea_m2 <- rep(calcChamberGeometryCylinder (radius = 0.1016,
                                                           height = soilH$havg_m,
                                                           taper  = 1.0) [dim(soilH)[1]+1], length(soilH[,1]))

stemH$vol_m3 <- calcChamberGeometryCylinder (radius = 0.0508,
                                             height = stemH$havg_m,
                                             taper  = 1.0) [-(dim(stemH)[1]+1)] #remove the last value 

stemH$respArea_m2 <- rep(calcChamberGeometryCylinder (radius = 0.0508,
                                                      height = stemH$havg_m,
                                                      taper  = 1.0) [dim(stemH)[1]+1], length(stemH[,1]))



# Read in a function that will plot *truncated* data (allows you to visually get rid of the noise in plots)
#Choose the start and end time (allows you to truncate the data to eliminate noise)
selectData <- function (ds = alldata, colConc = 'CO2', colTime = 'RunTime', lowerBound = 0, upperBound = 1e6) {
  # make sure upperBound is not higher than the length of the time series
  upperBound <- min (upperBound, max (ds [[colTime]]))
  # make sure lowerBound is not lower than the starting point of the time series
  lowerBound <- max (lowerBound, min (ds [[colTime]]))
  # select appropriate data
  dat <- ds [(ds [[colTime]] >= lowerBound) & 
               (ds [[colTime]] <= upperBound), ]
  cut <- ds [(ds [[colTime]] < lowerBound) | 
               (ds [[colTime]] > upperBound), ]
  # plot data
  plot (x = ds [,7], 
        y = ds [,8],
        xlab = 'time [s]',
        ylab = 'CO2 concentration [ppm]')
  # plot selected data bounds
  points (x = dat [[colTime]],
          y = dat [[colConc]],
          col  = '#91b9a499',
          pch  = 19,
          cex  = 0.9)
  rect (xleft   = lowerBound, 
        xright  = upperBound,
        ybottom = 0,
        ytop    = 100000,
        col     = '#91b9a411',
        border  = '#901C3B')
  # plot highlight cut data
  points (x = cut [[colTime]],
          y = cut [[colConc]],
          col  = '#901C3B99',
          pch  = 19,
          cex  = 0.9)
  rect (xleft   = 0, 
        xright  = lowerBound,
        ybottom = 0,
        ytop    = 100000,
        col     = '#901C3B11',
        lty     = 0)
  return (dat)
}
