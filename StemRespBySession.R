#this script will plot upper/lower bounds & calculate the flux (resp rate) of ea. chamber

#what session do you want to look at? 
#--------------------------------------------------------------------------------------#
date_time <- "20180626_1300"

calcSession <- function (date_time) {
#load source preprocess data (including chamber volume and bounds, plotting function)
#--------------------------------------------------------------------------------------#
require(segmented); require(tibble)
path<-"~/Documents/GitHub/Resp2018"
setwd(path); source('readdata.R')


path <- paste("~/Documents/HF REU/My Project/48HR/data/",date_time, sep='')
setwd(path)

myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substring(myfiles,1,9)=="G-Exp2018"]

tmp<-unlist(strsplit(myfiles,'_'))
datestr<-tmp[seq(3,length(tmp),4)]
timestr<-tmp[seq(4,length(tmp),4)]
timestr<-substring(timestr,1,nchar(timestr)-4)
timestamp<-strptime(paste(datestr,timestr),"%Y%m%d %H%M%S")

tree <- as.numeric(substring(myfiles,11,12))
chamber <- as.numeric(substring(myfiles,16,16))

while (length (chamber) != length (stemH$chamber)) {
  index <- which (stemH$tree != tree | stemH$chamber != chamber) [1]
  stemH <- stemH [-index, ]
  stemlowerbound <- stemlowerbound [-index]
  stemupperbound <- stemupperbound [-index]
}
avgh_cm<- stemH$havg_cm [stemH$chamber == chamber & stemH$tree == tree]
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


#Loop through each measurement in the session 
#--------------------------------------------------------------------------------------#
#pdf("InitialRespiration.pdf")
for (ifile in  1:nrow(sessiondata)){ #the curly bracket starts the loop
  
  #assign each of the files to a general variable "currentfile"
  currentfile<-sessiondata$file[ifile]
  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = currentfile, header = TRUE, dec = ".")
  
  dat <- selectData (ds= measurement,
                      lowerBound = stemlowerbound[ifile],
                      upperBound = stemupperbound[ifile]) 
  title(main = paste("Stem Respiration:",'tree',sessiondata$tree[ifile],'chamber',sessiondata$chamber[ifile],sessiondata$timestamp[ifile]))
  
  # to determine which weather measurements to use, we'll find the next 15 minute interval 
  next_interval <- as.POSIXct (x = (round (as.numeric (median (sessiondata$timestamp [ifile]))/
                                               (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                             origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                             tz = 'EST')
  
  pres.Pa <- met_HF$bar  [met_HF$TIMESTAMP == next_interval] * 100.0 # Pa
  airt.C  <- met_HF$airt [met_HF$TIMESTAMP == next_interval]         # deg C
  rh.per  <- met_HF$rh   [met_HF$TIMESTAMP == next_interval]         # %
  
 
  
  
  
  
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
                                   volume      = stemH$vol_m3[ifile],
                                   area        = stemH$respArea_m2[ifile])
  
  #Put all the data into the table 'sessiondata" you created earlier
  #--------------------------------------------------------------------------------------#
  fluxdata[[ifile]]<-resFit
  sessiondata$flux[ifile]<-resFit$flux
  sessiondata$sdFlux[ifile]<-resFit$sdFlux
  sessiondata$ea.Pa[ifile]   <- ea.Pa   # add actual water vapour pressure [Pa] to alldata data.frame
  sessiondata$airt.C[ifile]  <- airt.C   # add air temperature [degC] to alldata data.frame
  sessiondata$pres.Pa[ifile] <- pres.Pa   # add atmospheric pressure [Pa] to aalldat data.frame
  sessiondata$H2O.ppt[ifile] <- ea.Pa / (pres.Pa - ea.Pa) * 1.0e3   
}


#dev.off()
#flux units are micromol/sec. can use conver_mmol function to get grams/day

return (sessiondata)
}
