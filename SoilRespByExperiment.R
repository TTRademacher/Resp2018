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
#--------------------------------------------------------------------------------------#  
#Plot using dataframe "expdata"
#--------------------------------------------------------------------------------------#  

#pdf("soilRespRates.pdf")
#General, respiration rates per treatment over time (control = #41ae76, chill = #6BAED6)
plot(expdata$timestamp,expdata$flux,pch=1,main="Resp rates over time",
     col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"))

#Respiration rate per air temperature
#--------------------------------------------------------------------------------------#  
plot(expdata$airt.C,expdata$flux,pch=19,main="Resp rates per temperature",
     col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"))

#Soil Moisture
#--------------------------------------------------------------------------------------#  
par(mfrow=c(2,2))
  plot(expdata$soilWC1.C,expdata$flux,pch=19,main="Soil Moisture at Depth 1",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"),xlab="Soil Water Content (m^3/m^3)",ylab="Respiration Rate (μmol/sec)")
  plot(expdata$soilWC2.C,expdata$flux,pch=19,main="Soil Moisture at Depth 2",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"),xlab="Soil Water Content (m^3/m^3)",ylab="Respiration Rate (μmol/sec)")
  plot(expdata$soilWC3.C,expdata$flux,pch=19,main="Soil Moisture at Depth 3",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"),xlab="Soil Water Content (m^3/m^3)",ylab="Respiration Rate (μmol/sec)")
  plot(expdata$soilWC4.C,expdata$flux,pch=19,main="Soil Moisture at Depth 4",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"),xlab="Soil Water Content (m^3/m^3)",ylab="Respiration Rate (μmol/sec)")
  
  #Just the surface
  plot(expdata$soilWC1.C[expdata$treatment=="control"],expdata$flux[expdata$treatment=="control"],main="Soil Resp and Moisture at Surface",
       col="#41ae76",pch=19,xlab="Soil Water Content (m^3/m^3)",ylab="Respiration Rate (μmol/sec)")
  
  
#Soil Temperature
#--------------------------------------------------------------------------------------#  
par(mfrow=c(2,2))
  plot(expdata$soilT1.C,expdata$flux,pch=19,main="Soil Temp 1",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"))
  plot(expdata$soilT2.C,expdata$flux,pch=19,main="Soil Temp 2",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"))
  plot(expdata$soilT3.C,expdata$flux,pch=19,main="Soil Temp 3",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"))
  plot(expdata$soilT4.C,expdata$flux,pch=19,main="Soil Temp 4",
       col=ifelse (expdata$treatment=="chilling", "#6BAED6","#41ae76"))
  dev.off()
  
#Just the surface
  plot(expdata$soilT1.C[expdata$treatment=="control"],expdata$flux[expdata$treatment=="control"],main="Soil Resp and Temperature at Surface",
       col="#41ae76",pch=19,xlab="Soil Surface Temperature (C)",ylab="Respiration Rate (μmol/sec)")
  
  #abline(lm(expdata$soilT1.C[expdata$treatment=="control"]~expdata$flux[expdata$treatment=="control"]))
  
  
  
  
#48 HR exp, treatment-specific average respiration rates over time + standard error
#--------------------------------------------------------------------------------------#  

group<-paste(expdata$session,expdata$treatment)
flux_mean<-tapply(expdata$flux,group,mean)
flux_se<-tapply(expdata$flux,group,sd)/sqrt(tapply(expdata$flux,group,length)) #Standard error
tmp<-unlist(strsplit(names(flux_mean)," "))
treatment<-tmp[seq(2,length(tmp),2)]
session<-tmp[seq(1,length(tmp),2)]
session<-as.POSIXct(session,format="%Y%m%d_%H%M")
xlim <- c(as.POSIXct("20180625_0500",format="%Y%m%d_%H%M"),as.POSIXct( "20180627_1300",format="%Y%m%d_%H%M"))
ylim <- c(0,3)

plot(session,flux_mean,main="Soil Respiration - 48 Consecutive Hours",xlim=xlim,ylim=ylim,type="n",las=1,xlab = "Time (every two hours)",ylab = "Respiration Rate (μmol/sec)")  
  lines(session[treatment=="chilling"],flux_mean[treatment=="chilling"],col="#6BAED6")  
  lines(session[treatment!="chilling"],flux_mean[treatment!="chilling"],col="#41ae76")
  points(session[treatment=="chilling"],flux_mean[treatment=="chilling"],col="#6BAED6",pch=1)
  points(session[treatment!="chilling"],flux_mean[treatment!="chilling"],col="#41ae76",pch=19)  
  
  segments(session[treatment=="chilling"],flux_mean[treatment=="chilling"]-flux_se[treatment=="chilling"],session[treatment=="chilling"],flux_mean[treatment=="chilling"]+flux_se[treatment=="chilling"],col="#6BAED6")  
  segments(session[treatment!="chilling"],flux_mean[treatment!="chilling"]-flux_se[treatment!="chilling"],session[treatment!="chilling"],flux_mean[treatment!="chilling"]+flux_se[treatment!="chilling"],col="#41ae76")  
  legend("bottomright",c("Control","Chilling"),col=c("#41ae76","#6BAED6"),inset=0.02,pch=c(19,1),box.lty = 0)
  abline(v=as.POSIXct('20180625_1300', format = '%Y%m%d_%H%M'),
         col='black',
         lwd = 4,
         lty = 3)


  #Treatment-specific, plot average respiration rates over time + standard error
#--------------------------------------------------------------------------------------#  
  group<-paste(strftime(expdata$timestamp,"%j"),expdata$treatment)
  flux_mean<-tapply(expdata$flux,group,mean)
  flux_se<-tapply(expdata$flux,group,sd)/sqrt(tapply(expdata$flux,group,length)) #Standard error
  tmp<-unlist(strsplit(names(flux_mean)," "))
  day<-as.numeric(tmp[seq(1,length(tmp),2)])
  treatment<-tmp[seq(2,length(tmp),2)]
  
    plot(day,flux_mean,main="Soil Respiration since Chilling",ylab = "Respiration Rate (μmol/sec)",las=1,xlab = "Day of the Year")  
      lines(day[treatment=="chilling"],flux_mean[treatment=="chilling"],col="#6BAED6",lty=2)  
      lines(day[treatment!="chilling"],flux_mean[treatment!="chilling"],col="#41ae76")  
      segments(day[treatment=="chilling"],flux_mean[treatment=="chilling"]-flux_se[treatment=="chilling"],day[treatment=="chilling"],flux_mean[treatment=="chilling"]+flux_se[treatment=="chilling"],col="#6BAED6")  
      segments(day[treatment!="chilling"],flux_mean[treatment!="chilling"]-flux_se[treatment!="chilling"],day[treatment!="chilling"],flux_mean[treatment!="chilling"]+flux_se[treatment!="chilling"],col="#41ae76")  
      legend("topright",c("Control","Chilling"),col=c("#41ae76","#6BAED6"),inset=0.02,pch=c(19,1),box.lty = 0)
      abline(v=as.POSIXct('176', format = '%j'),
             col='black',
             lwd = 4,
             lty = 3)

      par (new = T)
      expdata$POSIXdate <- as.POSIXct(expdata$timestamp, '%Y-%m-%d $H:$M')
      plot (x = expdata$POSIXdate, y = expdata$flux,
            col = 'red',
            type = 'l',
            xlab = "n",
            ylab = "n")
        
      
      
#Adjusting for (air) temperature
#--------------------------------------------------------------------------------------#  
      for (tree in sort(unique(expdata$tree))){
        ord<-order(expdata$airt.C[expdata$tree==tree])
        lines(expdata$airt.C[expdata$tree==tree][ord],log(expdata$flux[expdata$tree==tree][ord]))
        model<-lm(log(expdata$flux[expdata$tree==tree])~expdata$airt.C[expdata$tree==tree])
        abline(model)
        Sys.sleep(1)
      }
      model<-lm(log(expdata$flux)~expdata$airt.C)
      abline(model,lwd=5,col="pink")
      # We take only the controls for temperature correction
      model<-lm(log(expdata$flux[expdata$treatment=="control"])~expdata$airt.C[expdata$treatment=="control"])
      abline(model,lwd=5)
      summary(model)
      expdata$flux_tc<- exp(log(expdata$flux) -(expdata$airt.C-20)*coefficients(model)[2])

#dev.off()
