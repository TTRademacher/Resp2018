


#what session do you want to look at? 
date_time <- "20180625_1300"

#load source preprocess data (including chamber volume and bounds, plotting function)
path<-"~/Documents/HF REU/My Project/48HR"
setwd(path)
source('readdata.R')

######### MAIN CODE #########

# let's break down the file name - flux puppy saved the file w/ the experiment name, the ID, the date, and the time. 
# First, locate the files
path<-paste("~/Documents/HF REU/My Project/48HR/",date_time, sep='')
setwd(path)
# make sure you draw upon only csv files (in case you save something else in there, like .R or html)
myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substr(myfiles,1,14)=="G-SoilResp2018"]

tree<- as.numeric(substring(myfiles,16,17))
chamber<- as.numeric(substring(myfiles,19,19))

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
samplingDate  <- as.POSIXct (datestr[1], format = '%Y%m%d')
#maybe to loop? might run meteorological data over and over tho
#samplingDate  <- as.POSIXct (datestr, format = '%Y%m%d') 

# put all of that together into a data frame, seperating all the elements into nice columns :)
alldata<-data.frame(file=myfiles,treatment=treatment,tree=tree,chamber=chamber,timestamp=timestamp,stringsAsFactors = FALSE)

# We need to account for other factors - humidity, atmospheric temperature, and pressure)
# Get appropriate meterological data from the harvard forest website

#first, I'll define the first of the month in which the sample was taken (that's the cutoff b/w archived and current weather data)
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
alldata$ea.Pa   <- NA   # add actual water vapour pressure [Pa] to alldata data.frame (blank columns)
alldata$airt.C  <- NA   # add air temperature [degC] to alldata data.frame
alldata$pres.Pa <- NA   # add atmospheric pressure [Pa] to aalldat data.frame
alldata$H2O.ppt <- NA   # add actual water vapour pressure [ppt] to alldata data.frame


#pdf("InitialRespiration.pdf")
# use a 'for loop', assigning "i" to represent each of the files in alldata 1 through "nrow", meaning all the rows
# Loop through flux puppy files 
for (ifile in  1:nrow(alldata)){ #the curly bracket starts the loop
  
  #assign each of the files to a general variable "currentfile"
  currentfile<-alldata$file[ifile]
  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = currentfile, header = TRUE, dec = ".")
  # to see what the data set looks like:
  measurement$RunTime<-measurement$RunTime*10
  
  
dat <- selectData (ds= measurement,
                    lowerBound = lowerbound[ifile],
                    upperBound = upperbound[ifile]) 
title(main = paste("Soil Respiration:",'tree',alldata$tree[ifile],'chamber',alldata$chamber[ifile],alldata$timestamp[ifile]))

# to determine which weather measurements to use, we'll find 15 minute interval (consecutive; 12:29 = 12:30, 12:31=12:45)
next_interval <- as.POSIXct (x = (round (as.numeric (median (alldata$timestamp [ifile]))/
                                             (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                           origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                           tz = 'EST')

pres.Pa <- met_HF$bar  [met_HF$TIMESTAMP == next_interval] * 100.0 # Pa
airt.C  <- met_HF$airt [met_HF$TIMESTAMP == next_interval]         # deg C
rh.per  <- met_HF$rh   [met_HF$TIMESTAMP == next_interval]         # %


# Calculate saturation water vapour pressure (esat) to convert relative humidity
es.Pa <- 0.61078 * exp ((17.269 * airt.C) / (237.3 + airt.C)) * 1000 # saturated water pressure [Pa]
ea.Pa <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
dat$ea.Pa   <- rep (ea.Pa,   length (dat [, 1]))   # add actual water vapour pressure [Pa] to alldata data.frame
dat$airt.C  <- rep (airt.C,  length (dat [, 1]))   # add air temperature [degC] to alldata data.frame
dat$pres.Pa <- rep (pres.Pa, length (dat [, 1]))   # add atmospheric pressure [Pa] to aalldat data.frame
dat$H2O.ppt <- dat$ea.Pa / (dat$pres.Pa - dat$ea.Pa) * 1.0e3   # add actual water vapour pressure [ppt] to alldata data.frame

names(dat)[which(names(dat)=="CO2")]<-"CO2.ppm"
# Correct CO2 concentration for water vapour
dat$CO2.dry <- corrConcDilution (dat, 
                                 colConc   = 'CO2.ppm',
                                 colVapour = 'H2O.ppt')

# Calculate chamber flux for entire timeseries

resFit <- calcClosedChamberFlux (dat,
                                 colConc     = 'CO2.dry',
                                 colTime     = 'RunTime', # redundant
                                 colTemp     = 'airt.C',
                                 colPressure = 'pres.Pa',
                                 volume      = dimensions$vol_m3[ifile],
                                 area        = dimensions$respArea_m2[ifile])

fluxdata[[ifile]]<-resFit
alldata$flux[ifile]<-resFit$flux
alldata$sdFlux[ifile]<-resFit$sdFlux
alldata$ea.Pa[ifile]   <- ea.Pa   # add actual water vapour pressure [Pa] to alldata data.frame
alldata$airt.C[ifile]  <- airt.C   # add air temperature [degC] to alldata data.frame
alldata$pres.Pa[ifile] <- pres.Pa   # add atmospheric pressure [Pa] to aalldat data.frame
alldata$H2O.ppt[ifile] <- ea.Pa / (pres.Pa - ea.Pa) * 1.0e3   

#closeAllConnections()
}



#use row bind - combine two dataframes

#if it's new 

#dev.off()
#flux units are micromol/sec. can use conver_mmol function to get grams/day
