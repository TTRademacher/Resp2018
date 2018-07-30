#this script will combine multiple sessions of data

#Read in all the source data ("StemRespBySession" & "readdata")
#--------------------------------------------------------------------------------------#
path<-"~/Documents/GitHub/Resp2018"
setwd(path); source('StemRespBySession.R')

#--------------------------------------------------------------------------------------#
setwd("~/Documents/HF REU/My Project/48HR/data")
mysessions <- list.files()

mysessions <- mysessions[ !grepl("20180625_0700", mysessions) ] 
mysessions <- mysessions[ !grepl("20180625_1100", mysessions) ]
mysessions <- mysessions[ !grepl("20180625_1300", mysessions) ]
mysessions <- mysessions[ !grepl("20180625_1900", mysessions) ]
mysessions <- mysessions[ !grepl("20180626_0100", mysessions) ]
mysessions <- mysessions[ !grepl("20180626_0700", mysessions) ]
mysessions <- mysessions[ !grepl("20180626_1900", mysessions) ]
mysessions <- mysessions[ !grepl("20180626_2200", mysessions) ]
mysessions <- mysessions[ !grepl("20180627_0100", mysessions) ]
mysessions <- mysessions[ !grepl("20180627_0400", mysessions) ]
mysessions <- mysessions[ !grepl("20180627_0700", mysessions) ]

mysessions <- mysessions[ !grepl("20180629_1300", mysessions) ] #only soil
mysessions <- mysessions[ !grepl("20180712_1300", mysessions) ] #only soil
mysessions <- mysessions[ !grepl("20180719_1300", mysessions) ] #only soil
#Call upon the calcSession function sourced above to loop through every session
#--------------------------------------------------------------------------------------#
if (file.exists("stemexpdata.rds")) {
  stemexpdata<-readRDS("stemexpdata.rds") #this saves the data set, won't remake it every time
} else {
  for (i in  1:length(mysessions)){
    
    date_time<-mysessions[i]
    sessiondata<-calcSession(date_time)
      if(i==1){
        stemexpdata <- sessiondata
      }else{
        stemexpdata <- rbind(stemexpdata,sessiondata)
        }
      }
  saveRDS(stemexpdata, file = "stemexpdata.rds")
} 




#VWC = soil moisture, EC = electric conductivity 

#Plot using dataframe "stemexpdata"
#--------------------------------------------------------------------------------------#  

#pdf("stemRespRates.pdf")

#Plot average respiration rates over time, including standard error

group2 <- paste(stemexpdata$session,stemexpdata$treatment)
flux_mean2 <- tapply(stemexpdata$flux,group2, mean) 
flux_se2<-tapply(stemexpdata$flux,group2,sd)/sqrt(tapply(stemexpdata$flux,group2,length)) #Standard error
tmp <- unlist(strsplit(names(flux_mean2)," "))
session<-tmp[seq(1,length(tmp),2)]
session<-as.POSIXct(session,format="%Y%m%d_%H%M")
treatment<-tmp[seq(2,length(tmp),2)]

xlim<-c(as.POSIXct("20180625",format="%Y%m%d"),as.POSIXct("20180720",format="%Y%m%d"))
ylim<-c(0,0.015)
    plot(session,flux_mean2,main="Avg stem resp rates over time",xlim=xlim,ylim=ylim)  
      lines(session[treatment=="chilling"],flux_mean2[treatment=="chilling"],col="blue")  
      lines(session[treatment=="control"],flux_mean2[treatment=="control"],col="green")
      lines(session[treatment=="compress"],flux_mean2[treatment=="compress"],col="red")  
      segments(session[treatment=="chilling"],flux_mean2[treatment=="chilling"]-flux_se2[treatment=="chilling"],session[treatment=="chilling"],flux_mean2[treatment=="chilling"]+flux_se2[treatment=="chilling"],col="blue")  
      segments(session[treatment=="compress"],flux_mean2[treatment=="compress"]-flux_se2[treatment=="compress"],session[treatment=="compress"],flux_mean2[treatment=="compress"]+flux_se2[treatment=="compress"],col="red")  
      segments(session[treatment=="control"],flux_mean2[treatment=="control"]-flux_se2[treatment=="control"],session[treatment=="control"],flux_mean2[treatment=="control"]+flux_se2[treatment=="control"],col="green")  

#isolate chamber #2
    
    fluxch2 <- stemexpdata$flux [stemexpdata$chamber == 2]
    group <- group2 [stemexpdata$chamber == 2]
    flux_se<-tapply(fluxch2,group,sd)/sqrt(tapply(fluxch2,group,length)) #Standard error
    flux_mean <- tapply(fluxch2, group, mean) 
    
    xlim<-c(as.POSIXct("20180625",format="%Y%m%d"),as.POSIXct("20180627",format="%Y%m%d"))
    ylim<-c(0,0.015)  
        
    plot(session,flux_mean,main="CH 2 Avg stem resp rates over time",xlim=xlim,ylim=ylim)  
    lines(session[treatment=="chilling"],flux_mean[treatment=="chilling"],col="blue")  
    lines(session[treatment=="control"],flux_mean[treatment=="control"],col="green")
    lines(session[treatment=="compress"],flux_mean[treatment=="compress"],col="red")  
    segments(session[treatment=="chilling"],flux_mean[treatment=="chilling"]-flux_se[treatment=="chilling"],session[treatment=="chilling"],flux_mean[treatment=="chilling"]+flux_se[treatment=="chilling"],col="blue")  
    segments(session[treatment=="compress"],flux_mean[treatment=="compress"]-flux_se[treatment=="compress"],session[treatment=="compress"],flux_mean[treatment=="compress"]+flux_se[treatment=="compress"],col="red")  
    segments(session[treatment=="control"],flux_mean[treatment=="control"]-flux_se[treatment=="control"],session[treatment=="control"],flux_mean[treatment=="control"]+flux_se[treatment=="control"],col="green")  
    
    #isolate chamber #1
    
    fluxch1 <- stemexpdata$flux [stemexpdata$chamber == 1]
    group1 <- group2 [stemexpdata$chamber == 1]
    flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
    flux_mean1 <- tapply(fluxch1, group1, mean) 
    
  
    xlim<-c(as.POSIXct("20180625",format="%Y%m%d"),as.POSIXct("20180627",format="%Y%m%d"))
    ylim<-c(0,0.020)  
    
  
    plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
    lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue")  
    lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green")
    lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red")  
    segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
    segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
    segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
    
    
##############
    #isolate chamber #1
    
    fluxch1 <- stemexpdata$flux [stemexpdata$chamber == 1]
    group1 <- group2 [stemexpdata$chamber == 1]
    flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
    flux_mean1 <- tapply(fluxch1, group1, mean) 
    
    
    xlim<-c(as.POSIXct("20180625",format="%Y%m%d"),as.POSIXct("20180720",format="%Y%m%d"))
    ylim<-c(0,0.050)  
    
    
    plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
    lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue")  
    lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green")
    lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red")  
    segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
    segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
    segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
    
    #isolate chamber #2
    
    fluxch1 <- stemexpdata$flux [stemexpdata$chamber == 2]
    group1 <- group2 [stemexpdata$chamber == 2]
    flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
    flux_mean1 <- tapply(fluxch1, group1, mean) 
    
    
    
    #plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
    lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue",lty=2)  
    lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green",lty=2)
    lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red",lty=2)  
    segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
    segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
    segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
    
    #isolate chamber #3
    
    fluxch1 <- stemexpdata$flux [stemexpdata$chamber == 3]
    group1 <- group2 [stemexpdata$chamber == 3]
    flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
    flux_mean1 <- tapply(fluxch1, group1, mean) 
  
    
    #plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
    lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue",lty=3)  
    lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green",lty=3)
    lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red",lty=3)  
    segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
    segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
    segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
    
    
    
      

      #Temperature over time
      plot(stemexpdata$timestamp,stemexpdata$airt.C,pch=19,main="Temperature over time")
      
      #Respiration rate per temperature
      colors<-  ifelse (stemexpdata$treatment=="chilling", "blue","green")
      colors[stemexpdata$treatment=="compress"]<-"red"
      plot(stemexpdata$airt.C,stemexpdata$flux,pch=19,main="Resp rates per temperature",
           col=colors)
    
      plot(stemexpdata$airt.C,log(stemexpdata$flux),pch=19,main="Resp rates per temperature",
           col=colors)
      
      for (tree in sort(unique(stemexpdata$tree))){
        #ord<-order(stemexpdata$airt.C[stemexpdata$tree==tree])
        #lines(stemexpdata$airt.C[stemexpdata$tree==tree][ord],log(stemexpdata$flux[stemexpdata$tree==tree][ord]))
        model<-lm(log(stemexpdata$flux[stemexpdata$tree==tree])~stemexpdata$airt.C[stemexpdata$tree==tree])
        abline(model)
        #Sys.sleep(1)
      }
      model<-lm(log(stemexpdata$flux)~stemexpdata$airt.C)
      #abline(model,lwd=5,col="pink")
      # We take only the controls for temperature correction
      #model<-lm(log(stemexpdata$flux[stemexpdata$treatment=="control"])~stemexpdata$airt.C[stemexpdata$treatment=="control"])
      abline(model,lwd=5)
      summary(model)
      stemexpdata$flux_tc<- exp(log(stemexpdata$flux) -(stemexpdata$airt.C-20)*coefficients(model)[2])

      plot(stemexpdata$airt.C,stemexpdata$flux_tc,pch=19,main="Resp rates per temperature",col=colors)
      
      
#########
      #isolate chamber #1
      
      fluxch1 <- stemexpdata$flux_tc [stemexpdata$chamber == 1]
      group1 <- group2 [stemexpdata$chamber == 1]
      flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
      flux_mean1 <- tapply(fluxch1, group1, mean) 
      
      
      xlim<-c(as.POSIXct("20180625",format="%Y%m%d"),as.POSIXct("20180720",format="%Y%m%d"))
      ylim<-c(0,0.050)  
      
      
      plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
      lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue")  
      lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green")
      lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red")  
      segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
      segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
      segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
      
      #isolate chamber #2
      
      fluxch1 <- stemexpdata$flux_tc [stemexpdata$chamber == 2]
      group1 <- group2 [stemexpdata$chamber == 2]
      flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
      flux_mean1 <- tapply(fluxch1, group1, mean) 
      
      
      
      #plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
      lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue",lty=2)  
      lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green",lty=2)
      lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red",lty=2)  
      segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
      segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
      segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
      
      #isolate chamber #3
      
      fluxch1 <- stemexpdata$flux_tc [stemexpdata$chamber == 3]
      group1 <- group2 [stemexpdata$chamber == 3]
      flux_se1<-tapply(fluxch1,group1,sd)/sqrt(tapply(fluxch1,group1,length)) #Standard error
      flux_mean1 <- tapply(fluxch1, group1, mean) 
      
      
      #plot(session,flux_mean1,main="CH 1 Avg stem resp rates over time near start",xlim=xlim, ylim=ylim)  
      lines(session[treatment=="chilling"],flux_mean1[treatment=="chilling"],col="blue",lty=3)  
      lines(session[treatment=="control"],flux_mean1[treatment=="control"],col="green",lty=3)
      lines(session[treatment=="compress"],flux_mean1[treatment=="compress"],col="red",lty=3)  
      segments(session[treatment=="chilling"],flux_mean1[treatment=="chilling"]-flux_se1[treatment=="chilling"],session[treatment=="chilling"],flux_mean1[treatment=="chilling"]+flux_se1[treatment=="chilling"],col="blue")  
      segments(session[treatment=="compress"],flux_mean1[treatment=="compress"]-flux_se1[treatment=="compress"],session[treatment=="compress"],flux_mean1[treatment=="compress"]+flux_se1[treatment=="compress"],col="red")  
      segments(session[treatment=="control"],flux_mean1[treatment=="control"]-flux_se1[treatment=="control"],session[treatment=="control"],flux_mean1[treatment=="control"]+flux_se1[treatment=="control"],col="green")  
      
      
      
      #add trend line
      model.lin<-lm(stemexpdata$flux~stemexpdata$airt.C)
      summary(model.lin) #take a look at R2 and p-value
      abline(model.lin,col="red")
      
      model.exp<-lm(log(stemexpdata$flux)~stemexpdata$airt.C)
      summary(model.exp)
      abline(model.exp,col="black")
dev.off()

#Q10 temperature coeefiecient ->> 
#moisture content regression model 