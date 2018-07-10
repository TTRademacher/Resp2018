#this script allows you to input the desired date and time and produce a graph for each flux puppy file 
#then you can manually/visually pick lower and upper bounds, recording them in a spreadsheet to be read in later

#What session do you want to make bounds for? input date and time here
date_time <- "20180629_1300"


require(segmented)
require(tibble)
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

################ MAIN CODE 
getwd()
#locate the file
path <- paste("~/Documents/HF REU/My Project/48HR/",date_time, sep='')
setwd(path)

# make sure you draw upon only csv files (in case you save something else in there, like .R or html)
myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substr(myfiles,1,14)=="G-SoilResp2018"]

#isolate the tree number from the file name
tree<- as.numeric(substring(myfiles,16,17))
#isolate the chamber
chamber<- as.numeric(substring(myfiles,19,19))

#or we can isolate the different variables in the file name using a split function and an indicator
  #strsplit(myfiles,'_')

#label the treatments based on the tree number
treatment<-rep("chilling",length(tree)) #name them all 'chilling'
treatment[tree<=5]<-"control" #name any tree less than or equal to 5 'control'

#isolate the date (datestr) and time (timestr) of measurement from the file name
tmp<-unlist(strsplit(myfiles,'_'))
datestr<-tmp[seq(3,length(tmp),4)]
timestr<-tmp[seq(4,length(tmp),4)]
timestr<-substring(timestr,1,nchar(timestr)-4)

#change the date and time format
timestamp<-strptime(paste(datestr,timestr),"%Y%m%d %H%M%S")

#PUT THE SAME DATE HERE; it will be used to extract meterological data
samplingDate  <- as.POSIXct (datestr[1], format = '%Y%m%d')
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
weatherdate<-paste(substring(datestr[1],1,4),substring(datestr[1],5,6),'01',sep='-')

if (samplingDate < as.POSIXct (weatherdate, format = '%Y-%m-%d')) {
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


#pdf("InitialRespiration.pdf")
# use a for loop, assigning "i" to represent each of the files in alldata 1 through "nrow", meaning all the rows
for (i in  1:nrow(alldata)){ #the curly bracket starts the loop
  
  #assign each of the files to a general variable "currentfile"
  currentfile<-alldata$file[i]
  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = currentfile, header = TRUE, dec = ".")
  # to see what the data set looks like:
  measurement$RunTime<-measurement$RunTime
  
  # Plot the CO2 concentrations over time
  plot(x = measurement [,7], y = measurement [,8], main = paste("Soil Respiration:",'tree',alldata$tree[i],'chamber',alldata$chamber[i],alldata$timestamp[i]),xlab = "time [s]", ylab = "CO2 concentration [ppm]")
  
}
