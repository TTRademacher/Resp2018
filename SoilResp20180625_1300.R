
# install and load the packages ('segmented' and 'tibble') 
# follow the path tools>install packages>searchname>install, or 
#install.packages("segmented")
#install.packages("tibble")

library(segmented)
library(tibble)
# add the library 'RespChamberProc' package by sourcing 
setwd ('/Users/bdavis/Documents/HF REU/My Project/RespChamberProc/')
fileNames <- list.files (pattern = "*.R") [-c (9:10)]
res <- sapply (fileNames, source); rm (res)


# Choose the start and end time (allows you to truncate the data to eliminate noise)
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
  
   
  plot (x = measurement [,7], 
        y = measurement [,8],
        xlab = 'time [s]',
        ylab = 'CO2 concentration [ppm]',
        main = paste("Soil Respiration:",alldata$treatment[i],'tree',alldata$tree[i],', chamber',alldata$chamber[i]))
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

################ MAIN CODE 

# Tell R where to find the data 
getwd()
# let's break down the file name - flux puppy saved the file w/ the experiment name, the ID, the date, and the time. 
# First, locate the files
path<-"~/Documents/HF REU/My Project/48HR/20180625_1300"
setwd(path)
# make sure you draw upon only csv files (in case you save something else in there, like .R or html)
myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substr(myfiles,1,1)=="G"]

myfiles<-myfiles[31:70]
#isolate the part of the file name that matters; everything after "SoilResp2018"
substring(myfiles,16,50)
#isolate the tree + chamber ID
#ID<- substring(myfiles,16,19)
#isolate the tree
tree<- as.numeric(substring(myfiles,16,17))
#isolate the chamber
chamber<- as.numeric(substring(myfiles,19,19))
#we can also isolate the different variables in the file name using a split function and an indicator
  #strsplit(myfiles,'_')

#let's label the treatments based on the tree number.
treatment<-rep("chilling",length(tree))
treatment[tree<=5]<-"control"

#isolate the date (datestr) and time (timestr) of measurement
tmp<-unlist(strsplit(myfiles,'_'))
datestr<-tmp[seq(3,length(tmp),4)]
timestr<-tmp[seq(4,length(tmp),4)]
timestr<-substring(timestr,1,nchar(timestr)-4)

#change the date and time format
timestamp<-strptime(paste(datestr,timestr),"%Y%m%d %H%M%S")
#PUT THE SAME DATE HERE; it will be used to extract meterological data
samplingDate  <- as.POSIXct ('20180625', format = '%Y%m%d')
#maybe to loop? might run meteorological data over and over tho
#samplingDate  <- as.POSIXct (datestr, format = '%Y%m%d') 


# put all of that together into a data frame, seperating all the elements into nice columns :)
alldata<-data.frame(file=myfiles,treatment=treatment,tree=tree,chamber=chamber,timestamp=timestamp,stringsAsFactors = FALSE)

# Calculate the volume of the closed gas chamber; use the geo function created by Tim and input respective measurements: 
chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508,
                                                height = 0.1016,
                                                taper  = 1.0)

# We need to account for other factors - humidity, atmospheric temperature, and pressure)
# Get appropriate meterological data from the harvard forest website
if (samplingDate < as.POSIXct ('2018-06-01', format = '%Y-%m-%d')) {
  met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p00/hf001/hf001-10-15min-m.csv'))
} else if (samplingDate >= as.POSIXct ('2018-06-01', format = '%Y-%m-%d')) {
  met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/qfm.csv'))
}
met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime, 
                                format = '%Y-%m-%dT%H:%M',
                                tz = 'EST') 


fluxdata=list()
alldata$flux<-NA
alldata$sdFlux<-NA
alldata$ea.Pa   <- NA   # add actual water vapour pressure [Pa] to alldata data.frame
alldata$airt.C  <- NA   # add air temperature [degC] to alldata data.frame
alldata$pres.Pa <- NA   # add atmospheric pressure [Pa] to aalldat data.frame
alldata$H2O.ppt <- NA   # add actual water vapour pressure [ppt] to alldata data.frame

pdf("InitialRespiration.pdf")
# use a for loop, assigning "i" to represent each of the files in alldata 1 through "nrow", meaning all the rows
for (i in  1:nrow(alldata)){ #the curly bracket starts the loop
  
  #assign each of the files to a general variable "currentfile"
  currentfile<-alldata$file[i]
  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = currentfile, header = TRUE, dec = ".")
  # to see what the data set looks like:
  str(measurement)
  
  # Plot the CO2 concentrations over time
  #plot(x = measurement [,7], y = measurement [,8], main = paste("Soil Respiration:",alldata$treatment[i],'tree',alldata$tree[i],', chamber',alldata$chamber[i]),xlab = "time [s]", ylab = "CO2 concentration [ppm]")
  #Sys.sleep(0.5)
  
  
#look through each graph and set upper and lower bounds. 
  # record them in excel, then import as csv file
  setwd("~/Documents/HF REU/My Project/48HR")
bounds<-read.csv(file = 'SoilResp2018_UpperLower.csv', header = TRUE)  
lowerbound<-bounds[1,seq(2,ncol(bounds),2)]
upperbound<-bounds[1,seq(3,ncol(bounds),2)]

path<-"~/Documents/HF REU/My Project/48HR/20180625_1300"
setwd(path)

dat <- selectData (ds= measurement,
                    lowerBound = lowerbound[i],
                    upperBound = upperbound[i]) 

} #ends the loop
# end the pdf file 
dev.off()



# Determine which measurement to use, N.B. data is named after the time at the end of the 15 minute interval
upper_bound <- as.POSIXct (x      = round (as.numeric (median (alldata$timestamp [1]))/
                                             (15 * 60)) * (15 * 60) + (15 * 60), 
                           format = '%Y-%m-%d %H:%M:%S',
                           origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                           tz = 'EST')

pres.Pa <- met_HF$bar  [met_HF$TIMESTAMP == upper_bound] * 100.0 # Pa
airt.C  <- met_HF$airt [met_HF$TIMESTAMP == upper_bound]         # deg C
rh.per  <- met_HF$rh   [met_HF$TIMESTAMP == upper_bound]         # %


# Calculate saturation water vapour pressure (esat) to convert relative humidity
es.Pa <- 0.61078 * exp ((17.269 * airt.C) / (237.3 + airt.C)) * 1000 # saturated water pressure [Pa]
ea.Pa <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
dat$ea.Pa   <- rep (ea.Pa,   length (dat [, 1]))   # add actual water vapour pressure [Pa] to alldata data.frame
dat$airt.C  <- rep (airt.C,  length (dat [, 1]))   # add air temperature [degC] to alldata data.frame
dat$pres.Pa <- rep (pres.Pa, length (dat [, 1]))   # add atmospheric pressure [Pa] to aalldat data.frame
dat$H2O.ppt <- dat$ea.Pa / (dat$pres.Pa - dat$ea.Pa) * 1.0e3   # add actual water vapour pressure [ppt] to alldata data.frame


# Correct CO2 concentration for water vapour
dat$CO2.dry <- corrConcDilution (dat, 
                                 colConc   = 'CO2',
                                 colVapour = 'H2O.ppt')

# Calculate chamber flux for entire timeseries

resFit <- calcClosedChamberFlux (dat,
                                 colConc     = 'CO2.dry',
                                 colTime     = 'RunTime', # redundant
                                 colTemp     = 'airt.C',
                                 colPressure = 'pres.Pa',
                                 volume      = chamberGeometry [1],
                                 area        = chamberGeometry [2])



#flux units are micromol/sec. can use conver_mmol function to get grams/day
