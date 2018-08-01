#this script will plot upper/lower bounds & calculate the flux (resp rate) of ea. chamber

#what session do you want to look at? 
#--------------------------------------------------------------------------------------#
date_time <- "20180730_1300"

calcSession <- function (date_time) {
#load source preprocess data (including chamber volume, bounds, plotting function)
#--------------------------------------------------------------------------------------#
require(segmented); require(tibble)
setwd("~/Documents/GitHub/Resp2018"); source('readdata.R') 

barndata <- read.csv("Barn_TableTot.dat",skip=1,header=TRUE)
barnNames <- names(barndata) 
barndata <- read.csv("Barn_TableTot.dat",skip=4,header=FALSE)
colnames(barndata) <- barnNames
barndata$TIMESTAMP <- as.POSIXct (barndata$TIMESTAMP, format = '%Y-%m-%d %H:%M:%S', tz = 'EST')

path <- paste("~/Documents/HF REU/My Project/48HR/data/",date_time, sep='')
setwd(path)

myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substring(myfiles,1,14)=="G-SoilResp2018"]

tmp<-unlist(strsplit(myfiles,'_'))
datestr<-tmp[seq(3,length(tmp),4)]
timestr<-tmp[seq(4,length(tmp),4)]
timestr<-substring(timestr,1,nchar(timestr)-4)
timestamp<-strptime(paste(datestr,timestr),"%Y%m%d %H%M%S")

tree<- as.numeric(substring(myfiles,16,17))
chamber<- as.numeric(substring(myfiles,19,19))

while (length (chamber) != length (soilH$chamber)) {
  index <- which (soilH$chamber != chamber) [1]
  soilH <- soilH [-index, ]
  soillowerbound <- soillowerbound [-index]
  soilupperbound <- soilupperbound [-index]
}
avgh_cm<- soilH$havg_cm [soilH$chamber == chamber & soilH$tree == tree]
radius<- 0.1016 

chamberGeometry <- calcChamberGeometryCylinder (radius = radius,
                                                height = avgh_cm, 
                                                taper  = 1.0)

treatment<-rep("chilling",length(tree)) #name them all 'chilling'
treatment[tree>5]<-"compress" #names any tree less than or equal to 10 'compress'
treatment[tree>10]<-"control" #names any tree less than or equal to 5 'control'


#this samplying date will be used to extract the meteoroligical data
samplingDate  <- as.POSIXct (datestr[1], format = '%Y%m%d')

#put all of that together into a data frame, seperating all the elements into columns 
#--------------------------------------------------------------------------------------#
sessiondata<-data.frame(file=myfiles,
                        treatment=treatment,
                        tree=tree,
                        chamber=chamber,
                        timestamp=timestamp,
                        stringsAsFactors = FALSE)

# Pull appropriate meterological data from the HF website to account for those factors
#--------------------------------------------------------------------------------------#
weatherdate<-Sys.Date()
weatherdate<-paste(substring(weatherdate,1,7),'01',sep='-')

if (samplingDate < as.POSIXct (weatherdate, format = '%Y-%m-%d')) {
  met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p00/hf001/hf001-10-15min-m.csv'))
} else if (samplingDate >= as.POSIXct (weatherdate, format = '%Y-%m-%d')) {
  met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/qfm.csv'))
}
met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime, 
                                format = '%Y-%m-%dT%H:%M',
                                tz = 'EST') 

# Set up the data table "sessiondata" to hold all the variables
#--------------------------------------------------------------------------------------#

fluxdata=list()
sessiondata$session<-date_time
sessiondata$flux<-NA
sessiondata$sdFlux<-NA
sessiondata$ea.Pa   <- NA  
sessiondata$airt.C  <- NA  
sessiondata$pres.Pa <- NA 
sessiondata$H2O.ppt <- NA 
sessiondata$soilT1.C <- NA
sessiondata$soilT2.C <- NA
sessiondata$soilT3.C <- NA
sessiondata$soilT4.C <- NA
sessiondata$soilWC1.C <- NA
sessiondata$soilWC2.C <- NA
sessiondata$soilWC3.C <- NA
sessiondata$soilWC4.C <- NA

#Loop through each measurement in the session 
#--------------------------------------------------------------------------------------#
#pdf("InitialRespiration.pdf")
for (ifile in  1:nrow(sessiondata)){ #the curly bracket starts the loop
  
  #assign each of the files to a general variable "currentfile"
  currentfile<-sessiondata$file[ifile]
  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = currentfile, header = TRUE, dec = ".")
  
  dat <- selectData (ds= measurement,
                      lowerBound = soillowerbound[ifile],
                      upperBound = soilupperbound[ifile]) 
  title(main = paste("SOIL Resp:",'tree',sessiondata$tree[ifile],'chamber',sessiondata$chamber[ifile],sessiondata$timestamp[ifile]))
  
  # to determine which weather measurements to use, we'll find 15 minute interval (consecutive; 12:29 = 12:30, 12:31=12:45)
  next_interval <- as.POSIXct (x = (round (as.numeric (median (sessiondata$timestamp [ifile]))/
                                               (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                             origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                             tz = 'EST')
  
  
  pres.Pa <- met_HF$bar  [met_HF$TIMESTAMP == next_interval] * 100.0 # Pa
  airt.C  <- met_HF$airt [met_HF$TIMESTAMP == next_interval]         # deg C
  rh.per  <- met_HF$rh   [met_HF$TIMESTAMP == next_interval]         # %
  
  #to determine which soil temp and moisture to use, find the next 10 minute interval 
  
  nextinterval2 <- as.POSIXct (x = (round (as.numeric (median (sessiondata$timestamp [ifile]))/
                                            (10 * 60)) * (10 * 60) + (10 * 60)), format = '%Y-%m-%d %H:%M:%S',
                              origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                              tz = 'EST')
 
  
  
  soilT1.C <- barndata$Soil_Temp_C_1 [barndata$TIMESTAMP == nextinterval2]
  soilT2.C <- barndata$Soil_Temp_C_2 [barndata$TIMESTAMP == nextinterval2]
  soilT3.C <- barndata$Soil_Temp_C_3 [barndata$TIMESTAMP == nextinterval2]
  soilT4.C <- barndata$Soil_Temp_C_4 [barndata$TIMESTAMP == nextinterval2]
  
  soilWC1.C <- barndata$VWC_1 [barndata$TIMESTAMP == nextinterval2]
  soilWC2.C <- barndata$VWC_2 [barndata$TIMESTAMP == nextinterval2]
  soilWC3.C <- barndata$VWC_3 [barndata$TIMESTAMP == nextinterval2]
  soilWC4.C <- barndata$VWC_4 [barndata$TIMESTAMP == nextinterval2]
  
  
  
  
  # Calculate saturation water vapour pressure (esat) to convert relative humidity
  #--------------------------------------------------------------------------------------#
  es.Pa <- 0.61078 * exp ((17.269 * airt.C) / (237.3 + airt.C)) * 1000 # saturated water pressure [Pa]
  ea.Pa <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
  dat$ea.Pa   <- rep (ea.Pa,   length (dat [, 1]))   # add actual water vapour pressure [Pa] to sessiondata data.frame
  dat$airt.C  <- rep (airt.C,  length (dat [, 1]))   # add air temperature [degC] to sessiondata data.frame
  dat$pres.Pa <- rep (pres.Pa, length (dat [, 1]))   # add atmospheric pressure [Pa] to aalldat data.frame
  dat$H2O.ppt <- dat$ea.Pa / (dat$pres.Pa - dat$ea.Pa) * 1.0e3   # add actual water vapour pressure [ppt] to sessiondata data.frame
  
  names(dat)[which(names(dat)=="CO2")]<-"CO2.ppm"
  # Correct CO2 concentration for water vapour
  dat$CO2.dry <- corrConcDilution (dat, 
                                   colConc   = 'CO2.ppm',
                                   colVapour = 'H2O.ppt')
  
  # Calculate chamber flux for entire timeseries
  #--------------------------------------------------------------------------------------#
  resFit <- calcClosedChamberFlux (dat,
                                   colConc     = 'CO2.dry',
                                   colTime     = 'RunTime', # redundant
                                   colTemp     = 'airt.C',
                                   colPressure = 'pres.Pa',
                                   volume      = soilH$vol_m3[ifile],
                                   area        = soilH$respArea_m2[ifile])
  
  #Put all the data into the table 'sessiondata" you created earlier
  #--------------------------------------------------------------------------------------#
  fluxdata[[ifile]]<-resFit
  sessiondata$flux[ifile]<-resFit$flux
  sessiondata$sdFlux[ifile]<-resFit$sdFlux
  sessiondata$ea.Pa[ifile]   <- ea.Pa   # add actual water vapour pressure [Pa] to alldata data.frame
  sessiondata$airt.C[ifile]  <- airt.C   # add air temperature [degC] to alldata data.frame
  sessiondata$pres.Pa[ifile] <- pres.Pa   # add atmospheric pressure [Pa] to aalldat data.frame
  sessiondata$H2O.ppt[ifile] <- ea.Pa / (pres.Pa - ea.Pa) * 1.0e3 
  sessiondata$soilT1.C[ifile] <- soilT1.C
  sessiondata$soilT2.C[ifile] <- soilT2.C
  sessiondata$soilT3.C[ifile] <- soilT3.C
  sessiondata$soilT4.C[ifile] <- soilT4.C
  sessiondata$soilWC1.C[ifile] <- soilWC1.C
  sessiondata$soilWC2.C[ifile] <- soilWC2.C
  sessiondata$soilWC3.C[ifile] <- soilWC3.C
  sessiondata$soilWC4.C[ifile] <- soilWC4.C
}


dev.off()
#flux units are micromol/sec. can use conver_mmol function to get grams/day

return (sessiondata)
}
