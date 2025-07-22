# Title: QAQC function for the FCR Catwalk
# Author: Adrienne Breef-Pilz
# Created: July 2020 (orignially call temp_oxy_chla_qaqc.R)
# Edited: 12 Feb 2024

# This function:
# 1. Read in the files and maintenance log
# 2. Fix time zone issues if reading in the raw files from 2019
# 3. Create Flag columns and flag missing values and those less than 0
# 4. Read in the Maintenance log and remove the values accordingly
# 5. Fill in non adjusted DO values
# 6. Remove and Flag when sensors are out of position or wiper is out of position
# 7. Leading and Lagging QAQC
# 8. Additional EXO algae sensor QAQC. 4 sd above the mean
# 9. Convert psi to depth for pressure sensor
# 10. Put everything in the right place
# 11. Write csv


qaqc_fcr <- function(data_file,
                     data2_file, 
                     maintenance_file, 
                     output_file = 'fcre-waterquality_L1.csv', 
                     start_date = NULL, 
                     end_date = NULL,
                    dir)
{

  a <-list.files(dir)
print(a)
  
  ### 1. Read in files and maintenance log ####
  # These are the column names for EDI 
  CATPRES_COL_NAMES = c("DateTime", "RECORD", "CR6Battery_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                        "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                        "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                        "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                        "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                        "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                        "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1","EXOTurbidity_FNU_1",
                        "EXOPressure_psi", "EXODepth_m", "EXOBattery_V", "EXOCablepower_V", "EXOWiper_V","LvlPressure_psi_9", "LvlTemp_C_9")
  
  #Adjustment period of time to stabilization after cleaning in seconds for DO sensor and for Temperature sensors
  ADJ_PERIOD_DO = 2*60*60 
  ADJ_PERIOD_Temp <- 60*30
  
  # The if statement is so we can use the function in the visual inspection script if we need
  # to qaqc historical data files from EDI
  
  if(is.character(data_file)){
    # read catwalk data and maintenance log
    # NOTE: date-times throughout this script are processed as UTC
    catdata <- read_csv(data_file, skip = 1, col_names = CATPRES_COL_NAMES,
                        col_types = cols(.default = col_double(), DateTime = col_datetime()))
  } else {
    
    catdata <- data_file
  }
  
  #read in manual data from the data logger to fill in missing gaps
  
  if(is.null(data2_file)){
    
    # If there is no manual files then set data2_file to NULL
    catdata2 <- NULL
    
  } else{
    
    catdata2 <- read_csv(data2_file, skip = 1, col_names = CATPRES_COL_NAMES,
                         col_types = cols(.default = col_double(), DateTime = col_datetime()))
  }
  
  # Bind the streaming data and the manual downloads together so we can get any missing observations 
  catdata <-bind_rows(catdata,catdata2)

   ### This was needed for the EDI publishing in 2021 and 
  # if you start with the all the raw files you will will need to run this section.
  # Need to run this section before removing duplicates
  
  # #### 2. Fix timezone issues when changed to EST #########
  # #Fix the timezone issues  
  # #time was changed from GMT-4 to GMT-5 on 15 APR 19 at 10:00
  # #have to seperate data frame by year and record because when the time was changed 10:00-10:40 were recorded twice
  # #once before the time change and once after so have to seperate and assign the right time. 
  
  if(is.null(output_file)){
    before<-catdata%>%
      filter(DateTime<ymd_hms("2019-04-15 10:50:00", tz="UTC"))%>%
      filter(DateTime<ymd_hms("2019-04-15 10:50:00", tz="UTC") & RECORD < 32879)
    
    
   # #now put into GMT-5 from GMT-4
    before$DateTime<-force_tz(as.POSIXct(before$DateTime), tz = "Etc/GMT+5") #get dates aligned
    before$DateTime<-with_tz(force_tz(before$DateTime,"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
    
    # list of RECORD obs that are already in the before section
    rec <- c(32874:32878)
    
    #filter after the time change 
    after=catdata%>%
      filter(DateTime>ymd_hms("2019-04-15 09:50:00", tz="UTC"))%>%
      # filter out the observations that were on the cusps and are in the before record
      filter(!(RECORD %in% rec & DateTime>ymd_hms("2019-04-15 09:50:00", tz="UTC") &DateTime<ymd_hms("2019-04-15 10:50:00", tz="UTC")))
      
    
    # Get all dates in the same timezone so they merge nicely
    after$DateTime<-force_tz(as.POSIXct(after$DateTime), tzone = "Etc/GMT+5")
    
    
    #merge before and after so they are one dataframe in GMT-5 
    catdata=bind_rows(before, after)
    
  }
  
  # There are going to be lots of duplicates so get rid of them
  catdata <- catdata[!duplicated(catdata$DateTime), ]
  
  #reorder 
  catdata <- catdata[order(catdata$DateTime),]
  
  #force tz check 
  catdata$DateTime <- force_tz(as.POSIXct(catdata$DateTime), tzone = "EST")
  
  # Take out the EXO_Date and EXO_Time column because we don't publish them 
  
  if("EXO_Date" %in% colnames(catdata)){
    catdata <- catdata%>%select(-c(EXO_Date, EXO_Time))
  }
  
  # convert NaN to NAs in the dataframe
  catdata[sapply(catdata, is.nan)] <- NA
  
  
  ## read in maintenance file 
  log <- read_csv2(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  #force tz check 
  log$TIMESTAMP_start <- force_tz(as.POSIXct(log$TIMESTAMP_start), tzone = "EST")
  log$TIMESTAMP_end <- force_tz(as.POSIXct(log$TIMESTAMP_end), tzone = "EST")
  
  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    #force tz check 
    start_date <- force_tz(as.POSIXct(start_date), tzone = "EST")
    
    catdata <- catdata %>% 
      filter(DateTime >= start_date)
    catdata2 <- catdata2 %>% 
      filter(DateTime >= start_date)
    log <- log %>% 
      filter(TIMESTAMP_start <= end_date)
  }
  
  if(!is.null(end_date)){
    #force tz check 
    end_date <- force_tz(as.POSIXct(end_date), tzone = "EST")
    
    catdata <- catdata %>% 
      filter(DateTime <= end_date)
    catdata2 <- catdata2 %>% 
      filter(DateTime <= end_date)
    log <- log %>% 
      filter(TIMESTAMP_end >= start_date)
  }
  
  ##### 3. Create Flag columns and flag missing values and ones less than 0 #####
  
  # remove NaN data at beginning when data when no sensors were connected to the data logger
  
  # Add this back in if you are working with the raw files
  # catdata <- catdata %>% filter(DateTime >= ymd_hms("2018-07-05 13:50:00"))
  
  
  # for loop to create flag columns
  for(j in colnames(catdata%>%select(ThermistorTemp_C_surface:LvlTemp_C_9))) { 
    #for loop to create new columns in data frame
    catdata[,paste0("Flag_",colnames(catdata[j]))] <- 0 #creates flag column + name of variable
    catdata[c(which(is.na(catdata[,j]))),paste0("Flag_",colnames(catdata[j]))] <-7 #puts in flag 7 if value not collected
  }
  
  # Change negative values to 0
  for(k in colnames(catdata%>%select(RDO_mgL_5, RDOsat_percent_5,RDO_mgL_9, RDOsat_percent_9,EXOCond_uScm_1:EXOTurbidity_FNU_1))) { 
    #for loop to create new columns in data frame
    catdata[c(which((catdata[,k]<0))),paste0("Flag_",colnames(catdata[k]))] <- 3
    catdata[c(which((catdata[,k]<0))),k] <- 0 #replaces value with 0
  }
  
  #add in the adjusted DO columns for used below as dates are in the maintenance log
  catdata <- catdata%>%
    mutate(
      RDO_mgL_5_adjusted = 0,
      RDOsat_percent_5_adjusted = 0,
      RDO_mgL_9_adjusted = 0,
      RDOsat_percent_9_adjusted = 0
    )
  
  #### 4. Maintenance Log QAQC#########
  
  
  # modify catdata based on the information in the log   

   if(nrow(log)==0){
     print('No Maintenance Events Found...')

   } else {
  
  for(i in 1:nrow(log))
  {
    ### get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    
    ### Get the Reservoir
    
    Reservoir <- log$Reservoir[i]
    
    ### Get the Site
    
    Site <- log$Site[i]
    
    ### Get the Maintenance Flag 
    
    flag <- log$flag[i]
    
    ### Get the Value or text that will be replaced
    
    update_value <- as.numeric(log$update_value[i])
    
    ### Get the code for fixing values. If it is not an NA
    
    if(flag==6){
      # These adjustment_code are expressions so they should not be set to numeric
      adjustment_code <- log$adjustment_code[i]
      
    }else{
      adjustment_code <- as.numeric(log$adjustment_code[i])
    }
    
    
    ### Get the names of the columns affected by maintenance
    
    colname_start <- log$start_parameter[i]
    colname_end <- log$end_parameter[i]
    
    ### if it is only one parameter parameter then only one column will be selected
    
    if(is.na(colname_start)){
      
      maintenance_cols <- colnames(catdata%>%select(any_of(colname_end))) 
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(catdata%>%select(any_of(colname_start)))
      
    }else{
      maintenance_cols <- colnames(catdata%>%select(c(colname_start:colname_end)))
    }
    
    
    ### Get the name of the flag column
    
    flag_cols <- paste0("Flag_", maintenance_cols)
    
    
    ### Getting the start and end time vector to fix. If the end time is NA then it will put NAs 
    # until the maintenance log is updated
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- catdata$DateTime >= start
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- catdata$DateTime <= end
      
    }else {
      
      Time <- catdata$DateTime >= start & catdata$DateTime <= end
      
    }
    
    ### This is where information in the maintenance log gets removed. Each flag has a different scenario so 
    # a flag that removes values can not also change a value. If there are different scenarios with in a data
    # flag then have a nested if statement with other identifying things such as specific columns for that situation. 
    
    # replace relevant data with NAs and set flags while maintenance was in effect
    if (flag==1){
      # The observations are changed to NA for maintenance or other issues found in the maintenance log
      catdata[Time, maintenance_cols] <- NA
      catdata[Time, flag_cols] <- flag
      
    } else if (flag==2){
      
      # The observations are changed to NA for maintenance or other issues found in the maintenance log
      catdata[Time, maintenance_cols] <- NA
      catdata[Time, flag_cols] <- flag
      
    } else if (flag==4){
      # Values are removed because they are out of range
      if(colname_start == "EXOChla_RFU_1" && colname_end== "EXOBGAPC_ugL_1"){
        
        Algae <- maintenance_cols[maintenance_cols%in%c("EXOChla_RFU_1", "EXOChla_ugL_1","EXOBGAPC_RFU_1","EXOBGAPC_ugL_1")]
        
        # for loop to loop through the columns from the algae sensor
        for(b in 1:length(Algae)){
          
          # get means and thresholds for using to find outliers
          
          mean <-mean(catdata[[Algae[b]]], na.rm=T)  
          
          threshold <- 4 * sd(catdata[[Algae[b]]], na.rm=T)
          
          # Set flag and take out files obs that are 4 sd above the mean
          
          catdata[c(which(Time & abs(catdata[[Algae[b]]] - mean) > threshold)),Algae[b]] <- NA
          
          # Add the flag 
          catdata[c(which(Time & abs(catdata[[Algae[b]]] - mean) > threshold)),paste0("Flag_",Algae[b])] <- flag
        }
      }else {
        # Set the values to NA and flag
        catdata[Time, maintenance_cols] <- NA
        catdata[Time, flag_cols] <- flag
      }
      
    } else if (flag==5){
      
      #Flag high conductivity values in 2020 but don't remove them. If want to remove later I will but I am
      # not convinced it is a sensor malfunction. Did everything based off of conductivity which is temperature
      # so catches anomalies as opposed to turnover which would happed if we used specific conductivity.  
      
      if(colname_start == "EXOCond_uScm_1" && colname_end== "EXOTDS_mgL_1"){
        
        # Make a vector of the Conductivity columns
        
        Cond <- maintenance_cols[maintenance_cols%in%c("EXOCond_uScm_1", "EXOSpCond_uScm_1", "EXOTDS_mgL_1")]
        
        for(s in 1:length(Cond)){
          catdata[c(which(Time & catdata[,"EXOCond_uScm_1"]>37 & catdata[,paste0("Flag_",Cond[s])]==0)),paste0("Flag_",Cond[s])] <- flag
        }
      }else{
        # Values are flagged but left in the dataset
        catdata[Time, flag_cols] <- flag
      }
    } else if (flag==6){ #adjusting the RDO_5_mgL
      
      # Adjusting the RDO sensors based on the CTD
      
      # Create a data frame of just time
      dt=catdata[Time, "DateTime"]
      
      # identify the new columns
      
      new_col <- paste0(maintenance_cols, "_adjusted")
      
      # put the adjusted value in a new column. The equation used to correct the value is in the maintenance log
      catdata[Time, new_col] <- catdata[Time, maintenance_cols] + eval(parse(text=adjustment_code))
      
      catdata[Time, flag_cols] <- flag
      
    }else if (flag==7){
      # Data was not collected and already flagged as NA above 
      
    }else if(flag==8){
      #  sensors off so made an offset correction which is in the maintenance log
      catdata[Time, maintenance_cols] <- catdata[Time, maintenance_cols] +adjustment_code
      
      catdata[Time, flag_cols] <- flag
      
    }else{
      # Flag is not in Maintenance Log
      warning(paste0("Flag", flag, "used not defined in the L1 script. 
                     Talk to Austin and Adrienne if you get this message"))
    }
    
    # Add the 2 hour adjustment for DO. This means values less than 2 hours after the DO sensor is out of the water are changed to NA and flagged
    # In 2023 added a 30 minute adjustment for Temp sensors on the temp string  
    
    # Make a vector of the DO columns
    
    DO <- colnames(catdata%>%select(grep("DO_mgL|DOsat", colnames(catdata))))
    
    # Vector of thermistors on the temp string 
    Temp <- colnames(catdata%>%select(grep("Thermistor|RDOTemp|LvlTemp", colnames(catdata))))
    
    # make a vector of the adjusted time
    Time_adj_DO <- catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD_DO
    
    Time_adj_Temp <- catdata$DateTime>start&catdata$DateTime<end+ADJ_PERIOD_Temp
    
    # Change values to NA after any maintenance for up to 2 hours for DO sensors
    
    if (flag ==1){
      
      # This is for DO add a 2 hour buffer after the DO sensor was out of the water
      catdata[Time_adj_DO,  maintenance_cols[maintenance_cols%in%DO]] <- NA
      catdata[Time_adj_DO, flag_cols[flag_cols%in%DO]] <- flag
      
      # Add a 30 minute buffer for when the temp string was out of the water
      catdata[Time_adj_Temp,  maintenance_cols[maintenance_cols%in%Temp]] <- NA
      catdata[Time_adj_Temp, flag_cols[flag_cols%in%Temp]] <- flag
    }
  }
 }    
  #### 5. Fill in non adjusted DO values ######
  # Fill in adjusted DO values that didn't get changed. If the value didn't get changed then it is the same as values from the 
  # non adjusted columns
  
  catdata=catdata%>%
    mutate(
      RDO_mgL_5_adjusted=ifelse(RDO_mgL_5_adjusted==0, RDO_mgL_5, RDO_mgL_5_adjusted),
      RDOsat_percent_5_adjusted=ifelse(RDOsat_percent_5_adjusted==0, RDOsat_percent_5, RDOsat_percent_5_adjusted),
      RDO_mgL_9_adjusted=ifelse(RDO_mgL_9_adjusted==0, RDO_mgL_9, RDO_mgL_9_adjusted),
      RDOsat_percent_9_adjusted=ifelse(RDOsat_percent_9_adjusted==0, RDOsat_percent_9, RDOsat_percent_9_adjusted)
    )
  
  ###### 6. Remove and Flag when sensors are out of position or wiper is out of position ######
  
  #change EXO values to NA if EXO depth is less than 0.5m and Flag as 2
  
  #index only the colummns with EXO at the beginning
  exo_idx <-grep("^EXO",colnames(catdata))
  
  
  #create list of the Flag columns that need to be changed to 2
  exo_flag <- grep("^Flag_EXO",colnames(catdata))
  
  # Change the EXO data to NAs when the EXO is above 0.5m and not due to maintenance
  #Flag the data that was removed with 2 for outliers
  catdata[which(catdata$EXODepth_m < 0.55 & !is.na(catdata$EXODepth_m)),exo_flag]<- 2
  catdata[which(catdata$EXODepth_m < 0.55 & !is.na(catdata$EXODepth_m)), exo_idx] <- NA
  
  
  # Flag the EXO data when the wiper isn't parked in the right position because it could be on the sensor when taking a reading
  #Flag the data that was removed with 2 for outliers
  catdata[which(catdata$EXOWiper_V < 1 & !is.na(catdata$EXOWiper_V)| catdata$EXOWiper_V > 1.35 & !is.na(catdata$EXOWiper_V)),exo_flag]<- 2
  # Take out values when wiper out of position
  catdata[which(catdata$EXOWiper_V < 1 & !is.na(catdata$EXOWiper_V)| catdata$EXOWiper_V > 1.35 & !is.na(catdata$EXOWiper_V)), exo_idx] <- NA
  
  
  
  #change the temp string and pressure sensor to NA if the psi is less than XXXXX and Flag as 2
  
  #index only the colummns with EXO at the beginning
  temp_idx <-grep("^Ther*|^RDO*|^Lvl*",colnames(catdata))
  
  #create list of the Flag columns that need to be changed to 2
  temp_flag <- grep("^Flag_Ther*|^Flag_RDO*|^Flag_Lvl*",colnames(catdata))
  
  #Change the EXO data to NAs when the pressure sensor is less than 9.94 psi which is roughly 7m and not due to maintenance
  #Flag the data that was removed with 2 for outliers
  catdata[which(catdata$LvlPressure_psi_9 < 9.94 & !is.na(catdata$LvlPressure_psi_9)),temp_flag]<- 2
  catdata[which(catdata$LvlPressure_psi_9 < 9.94 & !is.na(catdata$LvlPressure_psi_9)), temp_idx] <- NA
  
  
  ##### 7. Leading and Lagging QAQC #####
  # This finds the point that is way out of range from the leading and lagging point 
  
  # loops through all of the columns to catch values that are above 2 or 4 sd above or below
  # the leading or lagging point 
  
  # need to make it a data frame before
  
  catdata=data.frame(catdata)
  
  
  # Took out EXO depth, cable, battery, and wiper because those are more diagnotics and if there are outliers
  # then we have a larger problem that should be in the maintenance log
  
  for (a in colnames(catdata%>%select(ThermistorTemp_C_surface:EXOTurbidity_FNU_1, LvlPressure_psi_9:LvlTemp_C_9))){
    Var_mean <- mean(catdata[,a], na.rm = TRUE)
    
    # For Algae sensors we use 4 sd as a threshold but for the others we use 2
    if (colnames(catdata[a]) %in% c("EXOChla_RFU_1","EXOChla_ugL_1","EXOBGAPC_RFU_1","EXOBGAPC_ugL_1")){
      Var_threshold <- 4 * sd(catdata[,a], na.rm = TRUE)
    }else{ # all other variables we use 2 sd as a threshold
      Var_threshold <- 2 * sd(catdata[,a], na.rm = TRUE)
    }
    # Create the observation column, the lagging column and the leading column
    catdata$Var <- lag(catdata[,a], 0)
    catdata$Var_lag = lag(catdata[,a], 1)
    catdata$Var_lead = lead(catdata[,a], 1)
    
    # Replace the observations that are above the threshold with NA and then put a flag in the flag column
    
    catdata[c(which((abs(catdata$Var_lag - catdata$Var) > Var_threshold) &
                      (abs(catdata$Var_lead - catdata$Var) > Var_threshold)&!is.na(catdata$Var))) ,a] <-NA
    
    catdata[c(which((abs(catdata$Var_lag - catdata$Var) > Var_threshold) &
                      (abs(catdata$Var_lead - catdata$Var) > Var_threshold)&!is.na(catdata$Var))) ,paste0("Flag_",colnames(catdata[a]))]<-2
  }
  
  
  # Remove the leading and lagging columns
  
  catdata<-catdata%>%select(-c(Var, Var_lag, Var_lead))
  
  ##### 8. Additional EXO algae sensor QAQC. 4 sd above the mean ####
  # flag EXO sonde algae sensor data of value above 4 * standard deviation at other times but leave them in the dataset
  
  # Create a vector of just the columns on the algae sensor
  Algae <- c("EXOChla_RFU_1", "EXOChla_ugL_1","EXOBGAPC_RFU_1","EXOBGAPC_ugL_1")
  
  # for loop to loop through the columns from the algae sensor
  for(b in 1:length(Algae)){
    
    # get means and thresholds for using to find outliers
    
    mean <-mean(catdata[[Algae[b]]], na.rm=T)  
    
    threshold <- 4 * sd(catdata[[Algae[b]]], na.rm=T)
    
    # Flag with a 5 that the values are questionable but leave in dataset
    
    catdata[c(which(!is.na(catdata[[Algae[b]]]) & abs(catdata[[Algae[b]]] - mean) > threshold)),
            paste0("Flag_",Algae[b])] <- 5
  }
  #### 9. Convert psi to depth for pressure sensor ####
  
  #create depth column
  catdata <- catdata%>%mutate(LvlDepth_m_9=LvlPressure_psi_9*0.70455)#1psi=2.31ft, 1ft=0.305m
  
  #### 10. Put everything in the right place ####
  
  # add Reservoir and Site columns
  catdata$Reservoir <- "FCR"
  catdata$Site <- "50"
  
  
  # reorder columns
  catdata <- catdata %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_surface:ThermistorTemp_C_9,
                                RDO_mgL_5, RDOsat_percent_5,RDO_mgL_5_adjusted, RDOsat_percent_5_adjusted, 
                                RDOTemp_C_5, RDO_mgL_9, RDOsat_percent_9, 
                                RDO_mgL_9_adjusted, RDOsat_percent_9_adjusted, RDOTemp_C_9,
                                EXOTemp_C_1:LvlTemp_C_9, LvlDepth_m_9, RECORD, CR6Battery_V, CR6Panel_Temp_C,
                                Flag_ThermistorTemp_C_surface:Flag_ThermistorTemp_C_9,Flag_RDO_mgL_5:Flag_RDOTemp_C_9,
                                Flag_EXOTemp_C_1:Flag_EXOWiper_V, Flag_LvlPressure_psi_9, Flag_LvlTemp_C_9)
  
  #order by date and time
  catdata <- catdata[order(catdata$DateTime),]
  
  
  ## convert any data types that don't match EDI
  #catdata$DateTime <- as.Date(catdata$DateTime)
  
  ## convert flag columns to factor data types
  catdata <- catdata %>%
    mutate(across(starts_with("Flag"),
                  ~ as.factor(as.character(.))))
  
  #### 11. Write csv ####
  
  # write_csv was giving the wrong times. Let's see if this is better. 
  # If the output file is NULL then we are using it in a function and want the file returned and not saved. 
  if (is.null(output_file)){
    return(catdata)
  }else{
    # convert datetimes to characters so that they are properly formatted in the output file
   catdata$DateTime <- as.character(catdata$DateTime)
    write_csv(catdata, output_file)
  }
  print('QAQC output saved successfully')
}


# example usage

# qaqc_fcr(data_file= "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/fcre-waterquality.csv",
#     data2_file = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/fcre-waterquality.csv",
#     maintenance_file = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/FCR_CAT_MaintenanceLog.csv",
#     output_file = 'fcre-waterquality_L1.csv',
#     start_date = "2023-01-01 00:00:00",
#     end_date = Sys.Date())

