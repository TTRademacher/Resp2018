#this script allows you to input the desired date and time and produce a graph for each flux puppy file 
#then you can manually/visually pick lower and upper bounds, recording them in a spreadsheet to be read in later

#What session do you want to make bounds for? input date and time here
date_time <- "20180625_1300"

#add libraries and sources
require(segmented)
require(tibble)
setwd ('/Users/bdavis/Documents/HF REU/My Project/RespChamberProc/')
fileNames <- list.files (pattern = "*.R") [-c (9:10)]
res <- sapply (fileNames, source); rm (res)

#set path - change based on your filing system
path <- paste("~/Documents/HF REU/My Project/48HR/",date_time, sep='')
setwd(path)

# make sure you draw upon only csv files (in case you save something else in there, like .R or html)
myfiles<-list.files(getwd(),"csv")
myfiles<-myfiles[substring(myfiles,1,1)=="G"]

#this conditional statement will seperates Soil Resp vs Stem respiration (specific to our 2018 experiment)
for (i in  1:length(myfiles)) {
  myfile<-myfiles[i]
if(substr(myfile,1,14)=="G-SoilResp2018"){
     #isolate the tree number from the file name
   tree<- as.numeric(substring(myfile,16,17))
   #isolate the chamber
   chamber<- as.numeric(substring(myfile,19,19))
   chamberGeometry <- calcChamberGeometryCylinder (radius = 0.1000,
                                                  height = 0.045, #heights need to vary #make a function that spits out the volume into sessiondata
                                                  taper  = 1.0)
   ititle<-"SOIL RESP"
   
  }else if (substr(myfile,1,9)=="G-Exp2018"){
      #isolate the tree number from the file name
      tree<- as.numeric(substring(myfile,11,12))
      #isolate the chamber
      chamber<- as.numeric(substring(myfile,16,16))
      chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508,
                                                      height = 0.1016, #heights need to vary 
                                                      taper  = 1.0)
      ititle<-"STEM RESP"
  }

#or we can isolate the different variables in the file name using a split function and an indicator
  #strsplit(myfiles,'_')

#label the treatments based on the tree number
treatment<-rep("chilling",length(tree)) #name them all 'chilling'
treatment[tree<=10]<-"compress"
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

# put all of that together into a data frame, seperating all the elements into nice columns :)
sessiondata<-data.frame(file=myfiles,treatment=treatment,tree=tree,chamber=chamber,timestamp=timestamp,stringsAsFactors = FALSE)

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

#pdf("InitialRespiration.pdf")

  # read in the data file, and assign it to another generable variable "measurement"
  measurement<- read.csv(file = myfile, header = TRUE, dec = ".")

  # Plot the CO2 concentrations over time
  plot(x = measurement [,7], 
       y = measurement [,8], 
       main = paste(ititle,'tree',sessiondata$tree[i],'chamber',sessiondata$chamber[i],sessiondata$timestamp[i]),
       xlab = "time [s]", 
       ylab = "CO2 concentration [ppm]")
  
}
#dev.off()
