
# install and load the packages ('segmented' and 'tibble') 
# follow the path tools>install packages>searchname>install, or 

#install.packages("segmented")
#install.packages("tibble")

library(segmented)
library(tibble)
# add the library 'RespChamberProc' package by sourcing 
setwd ('/Users/bdavis/Documents/HF REU/My Project/RespChamberProc/')
fileNames <- list.files (pattern = "*.R") [-c (7:8)]
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
  plot (x = ds [[colTime]],
        y = ds [[colConc]],
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

# Tell R where to find the data 
getwd()
# let's break down the file name - flux puppy saved the file w/ the experiment name, the ID, the date, and the time. 
# First, locate the files
path<-"~/Documents/HF REU/My Project/48HR/20180625_1300"
setwd(path)
# make sure you draw upon only csv files (in case you save something else in there, like .R or html)
myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substr(myfiles,1,1)=="G"]

# Because every file name (or "string") starts with the same experiment name, we can isolate everything after it
# do this by making a sub string, starting at the 18th character; 50 is an arbitary number obviously bigger than the amount of characters there
substring(myfiles,18,50)

# That's cool - if every string was the same length we could use this "character counting" method to isolate each of the identifiers (exp name, site ID (direction + distance), date, and time) - BUT the strings vary in length (N vs NW). We'd have to either go in and manually rename the files (NN vs NW), OR split the string using useful symbol "_"
# split the string wherever the symbol "_" appears 
strsplit(myfiles,'_')

# the above will give you each of the file names (strings), and split them up, BUT they're still indexed by the full string (i.e. [1] would give us the entire first file name, not just the exp name of the first file name)
# we can assign each thing its own number/index by unlisting. unlist and assign a temporary variable:
tmp<-unlist(strsplit(myfiles,'_'))

# to isolate just one of the elements, we can use a sequence 
tmp<-tmp[seq(2,length(tmp),4)] # starting at the 2nd element of tmp above, stopping at the last (full length) of tmp, and produce a list of every 4 variables. *b/c each of the file names has four elements, this works. 
tmp<-unlist(strsplit(tmp,'p'))
tmp<-tmp[tmp!="x"]
tree<-as.numeric(substr(tmp,1,2))
tree<-tree[seq(1,length(tree),2)]
ring<-as.numeric(tmp[seq(2,length(tmp),2)])

treatment<-rep("compression",length(tree))
treatment[tree<=5]<-"chilling"
treatment[tree>10]<-"control"
length(tree)
tmp<-unlist(strsplit(myfiles,'_'))
datestr<-tmp[seq(3,length(tmp),4)]
timestr<-tmp[seq(4,length(tmp),4)]
timestr<-substring(timestr,1,nchar(timestr)-4)

date<-strptime(paste(datestr,timestr),"%Y%m%d %H%M%S")
samplingDate  <- as.POSIXct ('20180531', format = '%Y%m%d')


medium<-substr(myfiles,3,6)
medium[medium=="Exp2"]<-"Stem"

# put all of that together into a data frame, seperating all the elements into nice columns :)
alldata<-data.frame(file=myfiles,medium=medium,treatment=treatment,tree=tree,ring=ring,date=date,stringsAsFactors = FALSE)
str(alldata)


# Calculate the volume of the closed gas chamber; use the geo function created by Tim and input respective measurements: 
chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508,
                                                height = 0.1016,
                                                taper  = 1.0)

                     #### !!! How to make different geometry per medium !!!!! ###

# We need to account for other factors - humidity, atmospheric temperature, and pressure)
# Get appropriate meterological data from the harvard forest website
if (samplingDate < as.POSIXct ('2018-04-01', format = '%Y-%m-%d')) {
  met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p00/hf001/hf001-10-15min-m.csv'))
} else if (samplingDate >= as.POSIXct ('2018-04-01', format = '%Y-%m-%d')) {
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
  plot(x = measurement [,7], y = measurement [,8], main = paste("Respiration Rate",alldata$tree[i],alldata$ring[i]) ,xlab = "time [s]", ylab = "CO2 concentration [ppm]")
  #Sys.sleep(0.5)
  
  # each of the graphs has noise at different parts - look at each and manually enter start and end times for future trendline
lowerbound<-c(100,70,200,70,60,130,100,40,60,85,60,100,100,50,100,60,40,455,90,60,75,75,60,100,60,60,140,140,90,145,150,60,50,45,40,0,55,50,20,0,70,80,40,40,45,20,40,25,40,30,40,40,40,60,40,47,25,50,45,40,80,70,60,25,40,40,50,45,60,40)
upperbound<-c(180,140,260,140,150,250,180,120,130,160,120,170,160,200,160,130,120,500,150,130,145,135,140,160,120,140,200,200,150,200,210,120,120,100,120,130,115,90,120,75,130,135,100,100,105,90,110,85,110,110,110,105,110,140,100,107,90,95,100,100,120,160,120,80,110,120,95,95,115,100)
  
  #apply the upper and lower bounds now, using the function we defined earlier :D
 dat <- selectData (ds= measurement,
         lowerBound = lowerbound[i],
         upperBound = upperbound[i]) 
} #ends the loop
# end the pdf file 
dev.off()


# each of the graphs has noise at different parts - look at each and manually enter start and end times for future trendline
lowerbound<-c(100,70,200,70,60,130,100,40,60,85,60,100,100,50,100,60,40,455,90,60,75,75,60,100,60,60,140,140,90,145,150,60,50,45,40,0,55,50,20,0,70,80,40,40,45,20,40,25,40,30,40,40,40,60,40,47,25,50,45,40,80,70,60,25,40,40,50,45,60,40)
upperbound<-c(180,140,260,140,150,250,180,120,130,160,120,170,160,200,160,130,120,500,150,130,145,135,140,160,120,140,200,200,150,200,210,120,120,100,120,130,115,90,120,75,130,135,100,100,105,90,110,85,110,110,110,105,110,140,100,107,90,95,100,100,120,160,120,80,110,120,95,95,115,100)

lowerbound<-rep(200,nrow(alldata))
upperbound<-rep(5000,nrow(alldata))

# Determine which measurement to use, N.B. data is named after the time at the end of the 15 minute interval
upper_bound <- as.POSIXct (x      = round (as.numeric (median (alldata$date [1]))/
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
