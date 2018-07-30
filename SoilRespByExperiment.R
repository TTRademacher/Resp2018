#this script will combine multiple sessions of data

#Read in all the source data ("SoilRespBySession" & "readdata")
#--------------------------------------------------------------------------------------#
path<-"~/Documents/GitHub/Resp2018"
setwd(path); source('SoilRespBySession.R')

#--------------------------------------------------------------------------------------#
setwd("~/Documents/HF REU/My Project/48HR/data")
mysessions <- list.files()
mysessions <- mysessions[ !grepl("20180625_0900", mysessions) ] #only stem data
mysessions <- mysessions[ !grepl("20180626_0900", mysessions) ] #incomplete 
mysessions <- mysessions[ !grepl("20180713_1300", mysessions) ] #only stem data 
mysessions <- mysessions[ !grepl("20180720_1300", mysessions) ] #only stem data 


#Call upon the calcSession function sourced above to loop through every session
#--------------------------------------------------------------------------------------#
if (file.exists("expdata.rds")) {
  expdata<-readRDS("expdata.rds") #this saves the data set, won't remake it every time
} else {
  for (i in  1:length(mysessions)){
    
    date_time<-mysessions[i]
    sessiondata<-calcSession(date_time)
      if(i==1){
        expdata <- sessiondata
      }else{
        expdata <- rbind(expdata,sessiondata)
        }
      }
  saveRDS(expdata, file = "expdata.rds")
} 

#Plot using dataframe "expdata"
#--------------------------------------------------------------------------------------#  

pdf("soilRespRates.pdf")
#Respiration rates per treatment over time (control = green, chill = blue)
plot(expdata$timestamp,expdata$flux,pch=1,main="Resp rates over time",
     col=ifelse (expdata$treatment=="chilling", "blue","green"))

#Plot average respiration rates over time, including standard error
  group<-paste(strftime(expdata$timestamp,"%j"),expdata$treatment)
  flux_mean<-tapply(expdata$flux,group,mean)
  flux_se<-tapply(expdata$flux,group,sd)/sqrt(tapply(expdata$flux,group,length)) #Standard error
  tmp<-unlist(strsplit(names(flux_mean)," "))
  day<-as.numeric(tmp[seq(1,length(tmp),2)])
  treatment<-tmp[seq(2,length(tmp),2)]
  
    plot(day,flux_mean,main="Avg resp rates over time")  
      lines(day[treatment=="chilling"],flux_mean[treatment=="chilling"],col="blue")  
      lines(day[treatment!="chilling"],flux_mean[treatment!="chilling"],col="red")  
      segments(day[treatment=="chilling"],flux_mean[treatment=="chilling"]-flux_se[treatment=="chilling"],day[treatment=="chilling"],flux_mean[treatment=="chilling"]+flux_se[treatment=="chilling"],col="blue")  
      segments(day[treatment!="chilling"],flux_mean[treatment!="chilling"]-flux_se[treatment!="chilling"],day[treatment!="chilling"],flux_mean[treatment!="chilling"]+flux_se[treatment!="chilling"],col="red")  
    
      #Temperature over time
      plot(expdata$timestamp,expdata$airt.C,pch=19,main="Temperature over time")
      
      #Respiration rate per temperature
      plot(expdata$airt.C,expdata$flux,pch=19,main="Resp rates per temperature",
           col=ifelse (expdata$treatment=="chilling", "blue","green"))
      
      #add trend line
      model.lin<-lm(expdata$flux~expdata$airt.C)
      summary(model.lin) #take a look at R2 and p-value
      abline(model.lin,col="red")
      
      model.exp<-lm(log(expdata$flux)~expdata$airt.C)
      summary(model.exp)
      abline(model.exp,col="black")
dev.off()
