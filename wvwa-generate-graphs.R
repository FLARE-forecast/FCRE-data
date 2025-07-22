#script written to create daily figures that are sent to lucky SCC team members :)
#written by CCC, BB, Vahid-Dan, ABP

# Edits:
# 20 May 2025 - remove CCRE and BVRE graph generation
# 18 March 2024- add section for eddyflux fluxes
# 29 Sept. 2024- added in the water level to CCR
# 09 Dec. 2024 - added in an if statment to read in BVR file because added back in the data logger header. Add print statment when plots printed. 

continue_on_error <- function()
{
  print("ERROR! CONTINUING WITH THE REST OF THE SCRIPT ...")
}

options(error=continue_on_error)

#loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(Rcpp, generics, lubridate, tidyverse,gridExtra,openair, dplyr, magrittr, ggplot2, colorRamps,purrr)

# Set the timeout option to 500 seconds instead of 60
options(timeout=500)

download.file('https://github.com/FLARE-forecast/FCRE-data/raw/fcre-metstation-data/FCRmet.csv','FCRmet.csv')
download.file('https://github.com/FLARE-forecast/FCRE-data/raw/fcre-catwalk-data/fcre-waterquality.csv','fcre-waterquality.csv')
download.file('https://github.com/FLARE-forecast/FCRE-data/raw/fcre-weir-data/FCRweir.csv','FCRweir.csv')
#download.file('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/bvre-waterquality.csv','bvre-waterquality.csv')
download.file('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-eddyflux-data-qaqc/EddyFlux_streaming_L1.csv', 'Eddyflux.csv')

metheader<-read.csv("FCRmet.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
metdata<-read.csv("FCRmet.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(metdata)<-names(metheader) #combine the names to deal with Campbell logger formatting

metdata=metdata%>%
  filter(grepl("^20", TIMESTAMP))%>% #keep only the right TIMESTAMP rows 
  distinct(TIMESTAMP, .keep_all= TRUE) #taking out the duplicate values 

#take out the -INF and those above 1000 and when SRup and SRdown are NA
metdata=metdata%>%
  mutate(Albedo_Avg=ifelse(Albedo_Avg>1000, NA, Albedo_Avg),
         Albedo_Avg=ifelse(Albedo_Avg=="-Inf",NA, Albedo_Avg),
         Albedo_Avg=ifelse(is.na(SR01Up_Avg)|is.na(SR01Dn_Avg), NA, Albedo_Avg))

end.time <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H:%M")), tzone = "Etc/GMT+5") #gives us current time with rounded minutes in EDT
start.time <- end.time - days(7) #to give us seven days of data for looking at changes
full_time <- seq(start.time, end.time, by = "min") #create sequence of dates from past 5 days to fill in data

obs <- array(NA,dim=c(length(full_time),12)) #create array that will be filled in with 10 columns
#commented all lines that are irrelevant for 2020 data, per change in data downloads
#met_timechange=max(which(metdata$TIMESTAMP=="2019-04-15 10:19:00")) #shows time point when met station was switched from GMT -4 to GMT -5
metdata$TIMESTAMP<-as.POSIXct(strptime(metdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
#metdata$TIMESTAMP[c(1:met_timechange-1)]<-with_tz(force_tz(metdata$TIMESTAMP[c(1:met_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
#metdata=metdata[-c(met_timechange-1),]

if (length(na.omit(metdata$TIMESTAMP[metdata$TIMESTAMP>start.time]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("MetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time, "and", end.time, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
  print("FCR met file made with no data")
} else {
  for(i in 1:length(full_time)){ #this loop looks for matching dates and extracts data from metdata file to obs array
    index = which(metdata$TIMESTAMP==full_time[i])
    if(length(index)>0){
      obs[i,] <- unlist(metdata[index,c(1,2,3,5,8,9,10,11,13,14,15,18)])
    }
  }
  obs<-as.data.frame(obs) #make into DF
  colnames(obs)<-names(metdata[index,c(1,2,3,5,8,9,10,11,13,14,15,18)]) #get column names
  obs$TIMESTAMP<-full_time #now have your array with a proper timedate stamp!
  
  pdf(paste0("MetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  plot(obs$TIMESTAMP,obs$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
  plot(obs$TIMESTAMP,obs$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
  plot(obs$TIMESTAMP,obs$AirTC_Avg, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
  plot(obs$TIMESTAMP,obs$RH, main="Rel Hum", xlab="Time", ylab="%", type='l')
  plot(obs$TIMESTAMP,obs$Rain_mm_Tot, main="Rain", xlab="Time", ylab="mm", type='l')
  plot(obs$TIMESTAMP,obs$WS_ms_Avg, main="Wind speed", xlab="Time", ylab="m/s",type='l')
  plot(obs$TIMESTAMP,obs$SR01Up_Avg, main="Shortwave Up", xlab="Time", ylab="W/m2",type='l')
  plot(obs$TIMESTAMP,obs$SR01Dn_Avg, main="Shortwave Down", xlab="Time", ylab="W/m2",type='l')
  plot(obs$TIMESTAMP,obs$IR01UpCo_Avg, main="Longwave", xlab="Time", ylab="W/m2",type='l')
  plot(obs$TIMESTAMP,obs$PAR_Den_Avg, main="PAR", xlab="Time", ylab="umol/s/m^2",type='l')
  plot(obs$TIMESTAMP,obs$Albedo_Avg, main="Albedo", xlab="Time", ylab="W/m^2", type='l')
  dev.off() #file made!
  print("FCR met file made with data")
}


#time to now play with catwalk data!
#catheader<-read.csv("fcre-waterquality.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
catdata<-read.csv("fcre-waterquality.csv", skip=0, as.is=T) #get data minus wonky Campbell rows
#names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting

catdata=catdata%>%
  filter(grepl("^20", TIMESTAMP))%>% #keep only the right TIMESTAMP rows 
  distinct(TIMESTAMP, .keep_all= TRUE) #taking out the duplicate values 


end.time1 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded hours in EDT
start.time1 <- end.time1 - days(7) #to give us seven days of data for looking at changes
full_time1 <- as.data.frame(seq(start.time1, end.time1, by = "10 min")) #create sequence of dates from past 5 days to fill in data
colnames(full_time1)=c("TIMESTAMP") #make it a data frame to merge to make obs1 later

#obs1 <- array(NA,dim=c(length(full_time1),41)) #create array that will be filled in with 41 columns (the entire size of the array)
#cat_timechange=max(which(catdata$TIMESTAMP=="2019-04-15 10:00:00"))
catdata$TIMESTAMP<-as.POSIXct(strptime(catdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
#catdata$TIMESTAMP[c(1:cat_timechange-1)]<-with_tz(force_tz(catdata$TIMESTAMP[c(1:cat_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

# Get from the catwalk.csv while sending to the wrong file
#cat.time <- end.time1 - days(2)
#if (length(na.omit(catdata$TIMESTAMP[catdata$TIMESTAMP>cat.time]))==0) { 
#  download.file('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/Catwalk.csv', 'Catwalk2.csv')
#  catheader<-read.csv("fcre-waterquality.csv", skip=0, as.is=T) #get header minus wonky Campbell rows
#  catheader$wtr_pt_9<-NULL
#  catdata<-read.csv("Catwalk2.csv", skip=4, header =F) #get data minus wonky Campbell rows
#  names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting
#  catdata$TIMESTAMP<-as.POSIXct(strptime(catdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") 
#  }

if (length(na.omit(catdata$TIMESTAMP[catdata$TIMESTAMP>start.time1]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("CatwalkDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time1, "and", end.time1, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
  print("FCR catwalk file made with no data")
} else {
  #I would end up with NAs for all of the data values
  #for(j in 5:39){
  #catdata[,j]<-as.numeric(levels(catdata[,j]))[catdata[,j]]#need to set all columns to numeric values
  
  #}
  #for(i in 1:length(full_time1)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  #index = which(catdata$TIMESTAMP==full_time1[427])
  #if(length(index)>0){
  #obs1[n,] <- unlist(catdata[index,c(1:41)])
  #}
  #}
  
  obs1=merge(full_time1,catdata, all.x=TRUE)#merge the data frame to get the last 7 days
  obs1$Depth_m=obs1$Lvl_psi*0.70455 #converts pressure to depth
  #obs1<-as.data.frame(obs1) #make into DF
  #obs1[,1] <- full_time1 #now have your array with a proper timedate stamp!
  #colnames(obs1)<-names(catdata[index,c(1:41)]) #get column names
  
  pdf(paste0("FCRCatwalkDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  
  plot(obs1$TIMESTAMP,obs1$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
  if(min(tail(na.omit(obs1$BattV)))<11.5){
    mtext("Battery Charge Low", side = 3, col="red")
  }
  #added y limits so the axises would show up when the are no data
  plot(obs1$TIMESTAMP,obs1$EXO_battery, main="EXO Battery", xlab="Time", ylab="Volts", type='l', ylim=c(3,7))
  plot(obs1$TIMESTAMP,obs1$EXO_cablepower, main="EXO Cable Power", xlab="Time", ylab="Volts", type='l', ylim=c(10,15))
  plot(obs1$TIMESTAMP,obs1$EXO_depth, main="EXO Depth", xlab="Time", ylab="Meters", type='l', ylim=c(0,3))
  plot(obs1$TIMESTAMP, obs1$Depth_m, main="Bottom Sensor Depth", xlab="Time", ylab="Meters",type='l', ylim=c(9,11))
  plot(obs1$TIMESTAMP, obs1$EXO_wiper, main= "Wiper Voltage", xlab="Time", ylab="Volts", type='l')
  
  plot(obs1$TIMESTAMP,obs1$EXO_pressure, main="Sonde Pressure", xlab="Time", ylab="psi", type='l', ylim=c(-1,15))
  points(obs1$TIMESTAMP, obs1$Lvl_psi, col="blue4", type='l')
  legend("topleft", c("1.6m EXO", "9m PT"), text.col=c("black", "blue4"), x.intersp=0.001)
  
  #par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  plot(obs1$TIMESTAMP,obs1$dotemp_9, main="Water temp of sondes", xlab="Time", ylab="degrees C", type='l', col="medium sea green", lwd=1.5, ylim=c(0,35))
  points(obs1$TIMESTAMP, obs1$dotemp_5, col="black", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$EXO_wtr_1, col="magenta", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_pt_9, col="blue4", type='l', lwd=1.5)
  legend("topleft", c("1.6m EXO", "5m DO","9m DO", "9m PT"), text.col=c("magenta", "black","medium sea green", "blue4"), x.intersp=0.001)
  
  plot(obs1$TIMESTAMP,obs1$doobs_9, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim=c(min(obs1$doobs_9, obs1$doobs_5, obs1$doobs_1, na.rm = TRUE) - 1, max(obs1$doobs_9, obs1$doobs_5, obs1$doobs_1, na.rm = TRUE) + 5))
  points(obs1$TIMESTAMP, obs1$doobs_5, col="black", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$doobs_1, col="magenta", type='l', lwd=1.5)
  abline(h =2, col="red", lwd=3, lty=2)
  legend("topleft", c("1m", "5m", "9m", "2 mg/L"), text.col=c("magenta", "black","medium sea green", "red"), x.intersp=0.001)
  
  plot(obs1$TIMESTAMP,obs1$dosat_9, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim=c(min(obs1$dosat_9, obs1$dosat_5, obs1$dosat_1, na.rm = TRUE) - 1, max(obs1$dosat_9, obs1$dosat_5, obs1$dosat_1, na.rm = TRUE) + 10))
  points(obs1$TIMESTAMP, obs1$dosat_5, col="black", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$dosat_1, col="magenta", type='l', lwd=1.5)
  legend("topleft", c("1m", "5m","9m"), text.col=c("magenta", "black","medium sea green"), x.intersp=0.001)
  
  plot(obs1$TIMESTAMP,obs1$Cond_1, main="Cond, SpCond, TDS @ 1m", xlab="Time", ylab="uS/cm or mg/L", type='l', col="red", lwd=1.5, ylim=c(-0.5,60))
  points(obs1$TIMESTAMP, obs1$SpCond_1, col="black", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$TDS_1, col="orange", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$Turbidity_FNU_1, col="brown", type='l', lwd=1.5)
  legend("topleft", c("TDS", "SpCond", "Cond", "Turbidity"), text.col=c("orange", "black","red","brown"), x.intersp=0.001)
  
  plot(obs1$TIMESTAMP,obs1$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='l', col="green", lwd=1.5, ylim=c(min(obs1$BGAPC_1, na.rm = TRUE) - 2, max(obs1$Chla_1, na.rm = TRUE) + 5))
  points(obs1$TIMESTAMP, obs1$BGAPC_1, col="blue", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$fDOM_QSU_1, col="firebrick4", type='l', lwd=1.5)
  legend("topleft", c("Chla", "Phyco", "fDOM"), text.col=c("green", "blue", "firebrick4"), x.intersp=0.001)
  
  par(mfrow=c(1,1))
  par(oma=c(1,1,1,4))
  plot(obs1$TIMESTAMP,obs1$wtr_surface, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
  points(obs1$TIMESTAMP, obs1$wtr_1, col="firebrick1", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_2, col="DarkOrange1", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_3, col="gold", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_4, col="greenyellow", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_5, col="medium sea green", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_6, col="sea green", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_7, col="DeepSkyBlue4", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_8, col="blue2", type='l', lwd=1.5)
  points(obs1$TIMESTAMP, obs1$wtr_9, col="blue4", type='l', lwd=1.5)
  par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
         text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                    "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
  
  dev.off() #file made!
  print("FCR catwalk file made with data")
}


weirheader<-read.csv("FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
weirdata<-read.csv("FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(weirdata)<-names(weirheader) #combine the names to deal with Campbell logger formatting

weirdata=weirdata%>%
  filter(grepl("^20", TIMESTAMP))%>% #keep only the right TIMESTAMP rows 
  distinct(TIMESTAMP, .keep_all= TRUE) #taking out the duplicate values 

end.time2 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded minutes in EDT
start.time2 <- end.time2 - days(7) #to give us seven days of data for looking at changes
full_time2 <- as.data.frame(seq(start.time2, end.time2, by = "15 min")) #create sequence of dates from past 7 days to fill in data and make it a dataframe
colnames(full_time2)=c("TIMESTAMP") #make it a data frame to merge to make obs1 later

#obs2 <- array(NA,dim=c(length(full_time2),7)) #create array that will be filled in with 8 columns
weirdata$TIMESTAMP<-as.POSIXct(strptime(weirdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned

if (length(na.omit(weirdata$TIMESTAMP[weirdata$TIMESTAMP>start.time2]))<2) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("WeirDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time2, "and", end.time2, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
  print("FCR weir file made with no data")
} else { #else, do normal data wrangling and plotting
  #get columns
  #for(i in 1:length(full_time2)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  #index = which(weirdata$TIMESTAMP==full_time2[i])
  #if(length(index)>0){
  #obs2[i,] <- unlist(weirdata[index,c(1:7)])
  #}
  #}
  obs2=merge(full_time2,weirdata, all.x=TRUE)#merge the data frame to get the last 7 days
  #obs2<-as.data.frame(obs2) #make into DF
  #colnames(obs2)<-names(weirdata[index,c(1:7)]) #get column names
  #obs2$TIMESTAMP<-full_time2 #now have your array with a proper timedate stamp!
  
  #obs2$head=(0.149*obs2$Lvl_psi)/0.293 #equation as given by WW
  #obs2$head=(0.15/0.11)*obs2$Lvl_psi #updated equation by ABP from AGH 22SEP20
  obs2$head = ((80.534*obs2$Lvl_psi)+6.1945)/100 #updated equation by ABP from AGH 10FEB21
  obs2$flowcms= 2.391*(obs2$head^2.5) #original tidy code; obs2 <- obs2 %>%  mutate(head = (0.149*Lvl_psi)/0.293) %>% mutate(flow_cms = 2.391* (head^2.5))
  
  pdf(paste0("WeirDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  plot(obs2$TIMESTAMP,obs2$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
  plot(obs2$TIMESTAMP,obs2$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
  if(min(tail(na.omit(obs2$BattV)))<11.5){
    mtext("Battery Charge Low", side = 3, col="red")
  }
  plot(obs2$TIMESTAMP,obs2$AirTemp_C, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
  plot(obs2$TIMESTAMP,obs2$Lvl_psi, main="Water Level", xlab="Time", ylab="psi", type='l')
  plot(obs2$TIMESTAMP,obs2$wtr_weir, main="Water Temp", xlab="Time", ylab="degrees C", type='l')
  plot(obs2$TIMESTAMP,obs2$flowcms, main="Flow Rate", xlab="Time", ylab="cms", type='l')
  dev.off() #file made!
  print("FCR weir file made with data")
}

### EddyFlux plots
# Let's make the QAQC plots for streaming EddyFlux files

# Read in the data file
eflux<-read.csv("Eddyflux.csv") 

# Make the datetime column
eflux$datetime <- ymd_hms(paste0(eflux$date," ",eflux$time), tz="America/New_York")


eflux <- eflux%>%
  filter(grepl("^20", datetime))%>% #keep only the right TIMESTAMP rows 
  distinct(datetime, .keep_all= TRUE) #taking out the duplicate values 

end.time3 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "America/New_York") #gives us current time with rounded minutes in EDT
start.time3 <- end.time3 - days(7) #to give us seven days of data for looking at changes
full_time3 <- as.data.frame(seq(start.time3, end.time3, by = "30 min")) #create sequence of dates from past 7 days to fill in data and make it a dataframe
colnames(full_time3)=c("datetime") #make it a data frame to merge to make obs1 later


if (length(na.omit(eflux$datetime[eflux$datetime>start.time3]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("EddyFluxDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time2, "and", end.time2, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else { #else, do normal data wrangling and plotting
  
  # Make the data frame for the last 7 days
  obs6 <- merge(full_time3,eflux, all.x = TRUE)#merge the data frame to get the last 7 days
  
  
  # Plots
  pdf(paste0("EddyFluxDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  
  #Create the first page
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  
  text(5, 8, "EC Data Fluxes in Falling Creek Reservoir, Vinton, VA")
  text(5, 7, paste0("Start: ",obs6[1,'date']))
  
  
  # Now the plots for the first page
  p1=obs6 %>% 
    ggplot(aes(datetime, ch4_flux_umolm2s)) + geom_point() + geom_line() + theme_bw()
  
  p2=obs6 %>% 
    ggplot(aes(datetime, co2_flux_umolm2s)) + geom_point() + geom_line() + theme_bw()
  
  # CO2 and CH4 signal strength
  
  p3=obs6 %>% 
    ggplot(aes(datetime, rssi_77_mean)) + geom_point() + geom_line() +
    ylab("CH4 signal strength (%)")+
    theme_bw()
  
  #CO2 signal changes when took out the 7200
  if("co2_signal_strength_7200_mean" %in% names(obs6)) {
    p4=obs6 %>% 
      ggplot(aes(datetime, co2_signal_strength_7200_mean)) + geom_point() + geom_line() +
      ylab("CO2 signal strength (%)")+
      theme_bw()
  } else {
    p4=obs6 %>% 
      ggplot(aes(datetime, co2_signal_strength_7500_mean)) + geom_point() + geom_line() +
      ylab("CO2 signal strength (%)")+
      theme_bw()
  }
  
  #Smartflux voltage in 
  p5=obs6 %>% 
    ggplot(aes(datetime, vin_sf_mean)) + geom_point() +
    theme_bw() +
    geom_line() +
    xlab(" ") +
    ylab('Smartflux voltage (V)') +
    scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d") 
  theme(axis.text.x = element_text(size = 14, color = "black", angle = 60, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"))
  
  grid.arrange(p1,p2,p3,nrow=3)
  
  # plot ustar distribution, wind speed, windrose and temperature
  # Create a histogram of u*
  p6=obs6 %>% 
    ggplot(aes(x=u_star_ms)) + geom_histogram()+
    geom_histogram(color="black", fill="white")+
    xlab('u star (m s^-1)')+
    theme_bw()
  
  # Wind Speed U,V,W
  p7=obs6 %>% 
    ggplot(aes(datetime, u_var_ms)) + geom_point() + geom_line() +
    ylab("U (m/s)")+
    theme_bw()
  p8=obs6 %>% 
    ggplot(aes(datetime, v_var_ms)) + geom_point() + geom_line() +
    ylab("V (m/s)")+
    theme_bw()
  p9=obs6 %>% 
    ggplot(aes(datetime, w_var_ms)) + geom_point() + geom_line() +
    ylab("W (m/s)")+
    theme_bw()
  
  grid.arrange(p6,p7,p8,p9,nrow=4)
  
  # Visualize wind directions that 
  chicago_wind=obs6%>%
    select(datetime,wind_speed_ms,wind_dir)%>%
    dplyr::rename(date = datetime, ws = wind_speed_ms, wd = wind_dir)
  pollutionRose(chicago_wind, pollutant="ws")
  
  # Sonic Temperature
  p10=obs6 %>% mutate(sonicC=sonic_temperature_k-273.15)%>%
    ggplot(aes(datetime, sonicC)) + geom_point() + geom_line() +
    ylab("Sonic Temperature (Degrees C)")+
    theme_bw()
  
  # H and LE
  p11=obs6 %>% 
    ggplot(aes(datetime, H_wm2)) + geom_point() + 
    ylab("Sensible heat flux (W m^(-2))")+
    theme_bw()
  
  p12=obs6 %>% 
    ggplot(aes(datetime, LE_wm2)) + geom_point() +
    ylab("Latent heat flux (W m^(-2))")+
    theme_bw()
  
  grid.arrange(p11,p12,nrow=2)
  
  # Flow Rate
  if("flowrate_mean" %in% names(obs6)) {
    p13=obs6 %>%
      ggplot(aes(datetime, flowrate_mean*60000)) + geom_point() +
      theme_bw() +
      geom_line() +
      xlab(" ") +
      ylab('Flowrate (L min-1)') 
  } else {
    p13=obs6%>%
      ggplot(aes(datetime,60000))+geom_blank()+theme_bw()+
      labs(title="CO2 sensor is out for service and there is no flowrate")
  }
  grid.arrange(p10,p13,nrow=2)
  
  dev.off()
  print("EddyFlux file made")
}

# make CTD and Flora plots for Site 50 and upstream

ctd_data <- readr::read_csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv")

flora_data <- readr::read_csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv")

# function to make the plots
# function to make the plots
plot_var <- function(var_name,
                     fraction, 
                     reservoirs = c("FCR"),
                     data){
  
  recent <- data|>
    filter(Reservoir %in% reservoirs)|>
    filter(date(DateTime) == max(date(DateTime)))
  
  plotdate <- max(date(recent$DateTime))
  
  var <- sym(var_name)
  flag <- sym(paste0("Flag_", var_name))
  qaqc_plotly <- recent %>%
    filter(Reservoir %in% reservoirs, date(DateTime) %in% plotdate) %>%
    sample_frac(fraction)%>%
    ggplot(aes(x = Site, y = Depth_m, color = !!var)) +
    scale_color_gradientn(colours = blue2green2red(100), na.value="gray") +
    scale_y_reverse(limits = c(10, 0), breaks = seq(0, 10, by = 0.5)) +
    #scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 0.5))+
    geom_point() +
    ggtitle(print(paste0(var," ",plotdate))) +
    facet_grid(cols = vars(Reservoir))+
    theme_bw()
  
  
}

vars_to_plot_CTD <- c("Temp_C", "DO_mgL", "DOsat_percent", "Turbidity_NTU", "SpCond_uScm", "Chla_ugL", "pH", "PAR_umolm2s") 


vars_to_plot_flora <- c("TotalConc_ugL", "Bluegreens_ugL", "GreenAlgae_ugL", "BrownAlgae_ugL", "YellowSubstances_ugL", "MixedAlgae_ugL")

ctd_cast <- vars_to_plot_CTD %>%
  purrr::map(plot_var, fraction = 0.1, data = ctd_data) #specify date here


flora_cast <- vars_to_plot_flora%>%
  purrr::map(plot_var, fraction = 1 , data = flora_data)


# To save to file, here on A4 paper
ggsave(paste0("FCR_CTD_FloraDataFigures_", Sys.Date(), ".pdf"), plot = marrangeGrob(c(ctd_cast, flora_cast), nrow = 3, ncol =2), width = 8.5, height = 11, units = "in")



