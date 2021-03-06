#this script will plot upper/lower bounds & calculate the flux (resp rate) of ea. chamber

#what session do you want to look at? 
#--------------------------------------------------------------------------------------#
date_time <- "20180627_1300"

#load source preprocess data (including chamber volume and bounds, plotting function)
#--------------------------------------------------------------------------------------#
path<-"~/Documents/GitHub/Resp2018"
setwd(path)
source('Plots_chooseBounds.R')

#Loop through each measurement in the session 
#--------------------------------------------------------------------------------------#
#pdf("InitialRespiration.pdf")
for (ifile in  1:nrow(sessiondata)){ #the curly bracket starts the loop
  
  #assign each of the files to a general variable "currentfile"
  currentfile<-sessiondata$file[ifile]
  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = currentfile, header = TRUE, dec = ".")

dat <- selectData (ds= measurement,
                    lowerBound = lowerbound[ifile],
                    upperBound = upperbound[ifile]) 
title(main = paste("Soil Respiration:",'tree',sessiondata$tree[ifile],'chamber',sessiondata$chamber[ifile],sessiondata$timestamp[ifile]))

# to determine which weather measurements to use, we'll find 15 minute interval (consecutive; 12:29 = 12:30, 12:31=12:45)
next_interval <- as.POSIXct (x = (round (as.numeric (median (sessiondata$timestamp [ifile]))/
                                             (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                           origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                           tz = 'EST')


fluxdata=list()
sessiondata$flux<-NA
sessiondata$sdFlux<-NA
sessiondata$ea.Pa   <- NA   # add actual water vapour pressure [Pa] to alldata data.frame (blank columns)
sessiondata$airt.C  <- NA   # add air temperature [degC] to alldata data.frame
sessiondata$pres.Pa <- NA   # add atmospheric pressure [Pa] to aalldat data.frame
sessiondata$H2O.ppt <- NA   # add actual water vapour pressure [ppt] to alldata data.frame

pres.Pa <- met_HF$bar  [met_HF$TIMESTAMP == next_interval] * 100.0 # Pa
airt.C  <- met_HF$airt [met_HF$TIMESTAMP == next_interval]         # deg C
rh.per  <- met_HF$rh   [met_HF$TIMESTAMP == next_interval]         # %


# Calculate saturation water vapour pressure (esat) to convert relative humidity
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

resFit <- calcClosedChamberFlux (dat,
                                 colConc     = 'CO2.dry',
                                 colTime     = 'RunTime', # redundant
                                 colTemp     = 'airt.C',
                                 colPressure = 'pres.Pa',
                                 volume      = dimensions$vol_m3[ifile],
                                 area        = dimensions$respArea_m2[ifile])

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
