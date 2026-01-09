# Title: FCR Weir QAQC file
# Author: Adrienne Breef-Pilz
# Created: Feb. 2023
# Updated: 11 Jan 2025
# 09 Jan. 2026 took out the manual data file argument becasue it was redundant

# This function:
# 1. Reads in the VT streaming sensors, manual downloads from the VT streaming sensors, and the WVWA sensors
# 2. Combines them all together
# 3. Filters the files if there is a start_date and end_date
# 4. Read in maintenance log and use to flag or remove observations
# 5. The maintenance log also contains times for rating curves
# 6. Read in the staff gauge observations and create a table of staff gauge observations and pressure readings
# 7. Calculate the rating curve for each time period and calculate the flow
# 8. Save rating curve and file

qaqc_fcrweir <- function(VT_data_file = c('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/FCRweir.csv', 
                         'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/current_files/WeirData_L1.csv'), 
                         WVWA_data_file = 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_weirInflow_L1.csv', 
                         maintenance_file = 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/Weir_MaintenanceLog.csv', 
                         Staff_gauge_readings = 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/Inflow_Gauge_Height_at_Weir.csv', 
                         output_file, 
                         output_file_rating_curve,
                         start_date = NULL, 
                         end_date = NULL) {
  
  
  #### Read in the VT streaming sensors ####
  
  if (is.character(VT_data_file)==T) {
    VTsens<-read_csv(VT_data_file, skip=4,col_names=c("DateTime","Record","BattV","PTemp_C","AirTemp_C","Lvl_psi","wtr_weir"))
    
    VTdat <- VTsens%>%
      select(DateTime, Lvl_psi, wtr_weir)%>%
      dplyr::rename(VT_Pressure_psia = Lvl_psi,
                    VT_Temp_C = wtr_weir)
    
  }else {# If the file is used in the markdown file then is is already read in and nothing needs to happen
    VTdat<-VTsens
  }
  
  
  # Bind the streaming data with the manual downloads to fill in missing observations
  VTdat <-VTdat|>
    distinct()
  
  # take out the duplicate times
  VTdat <- VTdat[!duplicated(VTdat$DateTime), ]
  
  #reorder 
  VTdat<-VTdat[order(VTdat$DateTime),]
  
  # Add a column to calculate flow
  VTdat$VT_Flow_cms <- NA
  
  #### Read in the WVWA sensors ####
  WVWA_sens<- read_csv(WVWA_data_file)
  #read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2023_Jan/misc_data_files/WVWA_pressure_readings_2013_current.csv")
  
  
  # Add a column to calculate flow
  WVWA_sens$WVWA_Flow_cms <- NA
  
  #### read in maintenance file ####
  log_read <- #read_csv(file.path('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/Weir_MaintenanceLog.txt'),skip=0, col_types = cols(
    read_csv2(maintenance_file, skip=0, col_types = cols(
      .default = col_character(),
      TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
      TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
      flag = col_integer(),
    ))
  
  log <- log_read
  
  #force tz check 
  log$TIMESTAMP_start <- force_tz(as.POSIXct(log$TIMESTAMP_start), tzone = "EST")
  log$TIMESTAMP_end <- force_tz(as.POSIXct(log$TIMESTAMP_end), tzone = "EST")
  
  
  ### Combine the WVWA sensor and the VT sensor ######
  All_Inflow <- merge(WVWA_sens, VTdat, by='DateTime', all=TRUE)
  
  # Rearrange the columns
  
  All_Inflow<-All_Inflow%>%
    select("DateTime", "WVWA_Pressure_psi", "WVWA_Baro_pressure_psi", "WVWA_Pressure_psia",
           "WVWA_Flow_cms", "WVWA_Temp_C", "VT_Pressure_psia", "VT_Flow_cms", "VT_Temp_C")
  
  # Force timezone to EST
  
  All_Inflow$DateTime <- force_tz(as.POSIXct(All_Inflow$DateTime), tzone="EST")
  
  
  #### subset the maintenance log and the rating curve by the earliest and the latest dates needed for calculating the rating curve ####
  if (!is.null(start_date)){
    
    # force all time to EST
    start_date <- force_tz(as.POSIXct(start_date), tzone = "EST")
    
    rt_time <- log%>%
      filter(DataStream=="RATING")%>% # Rating curve times and observations that are needed for QAQC based on start_date
      filter(TIMESTAMP_end>=start_date)
    
    start_RC<-min(rt_time$TIMESTAMP_start) # get the minimum start date and time from the rating curves
    
    All_Inflow <- All_Inflow %>%
      filter(DateTime >= start_RC) # filter based on the rating curve start dates
    log <- log %>%
      filter(TIMESTAMP_end >= start_RC) # want info from the log that that hasn't ended by the start_date
  }
  
  if(!is.null(end_date)){
    
    # force all time to EST
    end_date <- force_tz(as.POSIXct(end_date), tzone = "EST")
    
    # get the maximum end date and time from the rating curves
    end_RC<-max(rt_time$TIMESTAMP_end) 
    
    All_Inflow <- All_Inflow %>%
      filter(DateTime <= end_RC) # filter file based on rating curve end dates
    log <- log %>%
      filter(TIMESTAMP_start <= end_RC) # want info from the log that that hasn't ended by the start_date
  }
  
  if (nrow(log) == 0){
    log <- log_read
  }
  
  
  
  #### Add Flag columns ###
  # for loop to create flag columns
  for(j in colnames(All_Inflow%>%select(WVWA_Pressure_psi:VT_Temp_C))) { #for loop to create new columns in data frame
    All_Inflow[,paste0("Flag_",j)] <- 0 #creates flag column + name of variable
    #All_Inflow[c(which(is.na(All_Inflow[,j]))),paste0("Flag_",j)] <-7 #puts in flag 7 if value not collected
  }
  
  
  #### QAQC with the Maintenance Log ####
  
  # separate the maintenance time from the dates and times for rating curves
  
  main_log <- log%>%
    filter(DataStream=="WEIR")
  
  ### QAQC based on Maintenance log ###
  
  # modify All_Inflow based on the information in the log
  
  for(i in 1:nrow(main_log)){
    ### get start and end time of one maintenance event
    start <- main_log$TIMESTAMP_start[i]
    end <- main_log$TIMESTAMP_end[i]
    
    
    ### Get the Reservoir
    
    Reservoir <- main_log$Reservoir[i]
    
    ### Get the Site
    
    Site <- main_log$Site[i]
    
    ### Get the Maintenance Flag 
    
    flag <- main_log$flag[i]
    
    ### Get the Value or text that will be replaced
    
    update_value <- as.numeric(main_log$update_value[i])
    
    ### Get the code for fixing values. If it is not an NA
    
      adjustment_code <- main_log$adjustment_code[i]
    
    
    ### Get the names of the columns affected by maintenance
    
    colname_start <- main_log$start_parameter[i]
    colname_end <- main_log$end_parameter[i]
    
    ### if it is only one parameter parameter then only one column will be selected
    
    if(is.na(colname_start)){
      
      maintenance_cols <- colnames(All_Inflow%>%select(any_of(colname_end))) 
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(All_Inflow%>%select(any_of(colname_start)))
      
    }else{
      maintenance_cols <- colnames(All_Inflow%>%select(c(colname_start:colname_end)))
    }
    
    
    ### Get the name of the flag column
    
    flag_cols <- paste0("Flag_", maintenance_cols)
    
    
    ### Getting the start and end time vector to fix. If the end time is NA then it will put NAs 
    # until the maintenance log is updated
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- All_Inflow$DateTime >= start
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- All_Inflow$DateTime <= end
      
    }else {
      
      Time <- All_Inflow$DateTime >= start & All_Inflow$DateTime <= end
      
    }
    
     # replace relevant data with NAs and set flags while maintenance was in effect
    if(flag==1){ # for down correcting
      if(!is.na(update_value)){
      All_Inflow[Time, maintenance_cols] <- All_Inflow[Time, maintenance_cols] + update_value
      All_Inflow[Time, flag_cols] <- flag
      
      # correct flags for those that were down corrected and if the flow is too low based on pressure sensor
      
      All_Inflow[which(Time & All_Inflow[,"WVWA_Pressure_psia"]<0.184), "Flag_WVWA_Flow_cms"] <- 13
      
      # correct flags for those that were down corrected and if the flow is too high and over tops the weir
      
      All_Inflow[which(Time & All_Inflow[,"WVWA_Pressure_psia"]>0.611), "Flag_WVWA_Flow_cms"] <- 16
      }else{

        # Use the code in the maintenace log to correct a value
        All_Inflow[Time, maintenance_cols] <- eval(parse(text=adjustment_code))
        
        All_Inflow[Time, flag_cols] <- flag
      }
      
    }
    
    else if(flag %in% c(2,4,5,7,8,14,24)){ # All other flags and change to NA
      All_Inflow[Time, maintenance_cols] <- NA
      All_Inflow[Time, flag_cols] <- flag
      
    }else if(flag %in% c(9)){
      # Just flag questionable values but don't remove them from the data frame

      All_Inflow[Time, flag_cols] <- flag
      
      
      }else{
      
      warning(paste0("Flag", flag, "used not defined in the L1 script. 
                     Talk to Austin and Adrienne if you get this message"))
    }
  }
  
  #### Read in gague height #####
  
  # Guage height csv
  # The file can either be on GitHub or Google Drive
  # Load in this spreadsheet and align with atmospheric corrected pressure measured by the WVWA sensor
  
  if(grepl("google", Staff_gauge_readings)){
    
    # The readings are from google Drive
    rating_curve_dates <- gsheet::gsheet2tbl(Staff_gauge_readings)|>
      select(Reservoir, Site, DateTime, GageHeight_cm)|>
      mutate(
      DateTime = lubridate::parse_date_time(DateTime, 
                                            orders = c('ymd HMS','ymd HM','ymd','mdy', 'mdy HM')))
    
  }else{
    # The readings are on GitHub 
   rating_curve_dates <- read_csv(Staff_gauge_readings)|>
      mutate(
        DateTime = lubridate::parse_date_time(DateTime, 
                                              orders = c('ymd HMS','ymd HM','ymd','mdy', 'mdy HM')))
  }
 
  
  # Round time to nearest 15 minutes
  
  rating_curve_dates <- rating_curve_dates %>%
    mutate(DateTime = round_date(DateTime,"15 minutes")) %>%
    select(DateTime,GageHeight_cm)
  
  rating_curve_dates$DateTime <- force_tz(as.POSIXct(rating_curve_dates$DateTime), tzone = "EST")
  
  
  rating_curve <- left_join(rating_curve_dates, All_Inflow, by="DateTime")
  rating_curve <- rating_curve%>%
    select(DateTime,GageHeight_cm, WVWA_Pressure_psia, VT_Pressure_psia)%>%
    drop_na(GageHeight_cm)
  # This is the data you will use to update the rating curve!
  # We will calculate a rating curve with data from each time period
  
  
  # Filter out the DateTime for making rating curve
  # The flag numbers are used for filtering out in the if statements
  rat_log <- log%>%
    filter(DataStream=="RATING")
  
  # Create a data frame for information about the rating curve and height for flow calculation can go
  
    header<- c("Sensor", "Start", "End", "Slope", "Intercept", "Low_psi", "High_psi")
    df2<- as.data.frame(setNames(replicate(7,numeric(0), simplify = F), header))
 
  
  ### Get Dates and Times for rating curve ####
    
    # Need to get flow first and then take out values. Maybe??
  
  # modify All_Inflow based on the information in the log
  for(i in 1:nrow(rat_log)){
    # get start and end time of one maintenance event
    start <- rat_log$TIMESTAMP_start[i]
    end <- rat_log$TIMESTAMP_end[i]
    
    ### Get the Maintenance Flag 
    
    flag <- rat_log$flag[i]
    
    
    ### Get the names of the columns affected by maintenance
    
    colname_start <- rat_log$start_parameter[i]
    colname_end <- rat_log$end_parameter[i]
    
    ### if it is only one parameter parameter then only one column will be selected
    
    if(is.na(colname_start)){
      
      maintenance_cols <- colnames(All_Inflow%>%select(any_of(colname_end))) 
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(All_Inflow%>%select(any_of(colname_start)))
      
    }else{
      maintenance_cols <- colnames(All_Inflow%>%select(c(colname_start:colname_end)))
    }
    
    
    ### Get the name of the flag column
    
    flag_cols <- paste0("Flag_", maintenance_cols)
    
    ### Get the name of the pressure column
    
    if(maintenance_cols=="WVWA_Flow_cms"){
      
      press <- "WVWA_Pressure_psia"
    } else if(maintenance_cols == "VT_Flow_cms"){
      
      press <- "VT_Pressure_psia"
    }else{
      
    }
    
    ### Getting the start and end time vector to fix. If the end time is NA then it will put NAs 
    # until the maintenance log is updated
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- All_Inflow$DateTime >= start
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- All_Inflow$DateTime <= end
      
    }else {
      
      Time <- All_Inflow$DateTime >= start & All_Inflow$DateTime <= end
      
    }
    
    # getting liner relationship between psi and guage height and calculating flow
    
    # The first one is labeled 100 
    if(flag==100){
      
      # the old weir equations are taken directly from MEL's Inflow Aggregation script
      # Use for pressure data prior to 2019-06-06: see notes above for description of equations
      # NOTE: Pressure_psia < 0.185 calculates -flows (and are automatically set to NA's)
      # THIS WILL NOT CHANGE EVER AGAIN! KEEP AS IS FOR RECTANGULAR WEIR
      
      All_Inflow[Time, "flow1"]<-All_Inflow[Time, press]* 0.70324961490205 - 0.1603375 + 0.03048
      All_Inflow[Time, "flow_cfs"]<- (0.62 * (2/3) * (1.1) * 4.43 * (All_Inflow[Time, "flow1"] ^ 1.5) * 35.3147)
      All_Inflow[Time, maintenance_cols]<-All_Inflow[Time, "flow_cfs"]*0.028316847
      
      # take out columns you don't need any more
      All_Inflow<-subset(All_Inflow, select=-c(flow1, flow_cfs))
      
      # correct flags for flow being too low based on pressure sensor
     
      All_Inflow[which(Time & All_Inflow[,flag_cols]!=13 & !is.na(All_Inflow[,press]) & All_Inflow[,press]<0.184), flag_cols] <- 3
      
      # Make flow NA when psi <= 0.184 (distance between pressure sensor and bottom of weir = 0.1298575 m = 0.18337 psi
      
      All_Inflow[which(Time & !is.na(All_Inflow[,press]) & All_Inflow[,press]<0.184), maintenance_cols] <- NA
      
      # Will also need to flag flows when water tops the weir: for the rectangular weir, head = 0.3 m + 0.1298575 m = 0.4298575 m
      # This corresponds to Pressure_psia <= 0.611244557965199
      # diff_pre$Flow_cms = ifelse(diff_pre$Pressure_psia > 0.611, NA, diff_pre$Flow_cms)
      # Decided to keep data from above the weir, but flag!
      
      All_Inflow[which(Time & !is.na(All_Inflow[,press]) & All_Inflow[,flag_cols]!=16 & All_Inflow[,press]>0.611), flag_cols] <-6
      
      
      # populate the data frame for the stats from the rating curve
      
      df2[i,"Sensor"]<-maintenance_cols
      df2[i, "Start"]<-as.character(format(start))
      df2[i, "End"]<-as.character(format(end))
      df2[i, "Slope"]<-NA
      df2[i, "Intercept"]<-NA
      df2[i, "Low_psi"]<-0.184
      df2[i, "High_psi"]<-0.611
      
      # q = 2.391 * H^2.5
      # where H = head in meters above the notch
      # the head was 14.8 cm on June 24 at ~13:30
      #14.8 cm is 0.148 m
      #14.9cm on Jun 27 at 3:49PM
      # WW: Used scaling relationship to determine head:pressure relationship
      # diff_post <- diff_post %>%  mutate(head = (0.149*Pressure_psia)/0.293) %>%
      #   mutate(Flow_cms = 2.391* (head^2.5)) %>%
      #   select(DateTime, Temp_C, Baro_pressure_psi, Pressure_psi, Pressure_psia, Flow_cms)
      
    } else if (flag!=100){
      # Can use this for the rest of the time because everything is done here
      # This is for WVWA sensor
      # figure out the liner regression
      
      # select rating curve based on date time
      sel<- rating_curve[rating_curve$DateTime >= start & rating_curve$DateTime <= end,]
      
      # check if there are observations for each column
      ind <- colSums(is.na(sel)) == nrow(sel)
      
      if(ind[[press]]==T){
        
        df2[i,"Sensor"]<-maintenance_cols
        df2[i, "Start"]<-as.character(format(start))
        df2[i, "End"]<-as.character(format(end))
        df2[i, "Slope"]<-"No observations"
        
        
      }else{
      
      # Let's calculate flow for WVWA
      #get linear relationship between pressure gage and
      fit <- lm(paste0('GageHeight_cm ', '~', press), data = sel, na.action=na.omit)
      #plot(sel_WVWA$GageHeight_cm ~ sel_WVWA$WVWA_Pressure_psia)
      # Pull out intercept
      int<-as.numeric(fit$coefficients[1])
      
      # Pull out slope
      slo<-as.numeric(fit$coefficients[2])
      
      # Calculate the Flow
      
      All_Inflow[Time,maintenance_cols] <- 2.391 * ((((All_Inflow[Time,press]*slo)+ int)/100)^2.5)
      
      
      # take out values when flow is too low to detect
      # figure out when that is
      low <- (-int)/slo
      
      # take out the low values and flag
      # According to gage height vs. pressure relationship as calculated above
      
      All_Inflow[which(Time & !is.na(All_Inflow[,press]) & All_Inflow[,press]<low),flag_cols] <- 3
      
      All_Inflow[which(Time & !is.na(All_Inflow[,press]) & All_Inflow[,press]<low),maintenance_cols] <- NA
      
      
      # flag when water is over top of the weir at 27.5 cm but not taking it out
      # figure out at what psia that is at
      high=(27.5-int)/slo
      
      # Flag the values when the water is going over the top of the weir

       if(flag_cols %in% 1){

          All_Inflow[which(Time & !is.na(All_Inflow[,press]) & All_Inflow[,press]>high),flag_cols] <- 16
         
       } else{
      
      All_Inflow[which(Time & !is.na(All_Inflow[,press]) & All_Inflow[,press]>high),flag_cols] <- 6

      }   
      
      # populate the data frame for the stats from the rating curve
      
        df2[i,"Sensor"]<-maintenance_cols
        df2[i, "Start"]<-as.character(format(start))
        df2[i, "End"]<-as.character(format(end))
        df2[i, "Slope"]<-slo
        df2[i, "Intercept"]<-int
        df2[i, "Low_psi"]<-low
        df2[i, "High_psi"]<-high
      }
        
    }else
      print(paste("Issue with creating the rating curve. Check row", i, "of the rating log."))
  }
  
  # Drop NA in the data frame of slope info
  
    df<-df2%>%
      drop_na(Sensor)%>%
      mutate_if(is.numeric, ~round(., 3))

  # Round the calculated columns in the All_Inflow file
  All_Inflow <- All_Inflow%>%
  mutate_if(is.numeric, ~round(., 3))
  
  
  # Now subset the data files to the only ones that are new
  if (!is.null(start_date)){
    All_Inflow <- All_Inflow %>%
      filter(DateTime >= start_date)
  }
  
  if(!is.null(end_date)){
    All_Inflow <- All_Inflow %>%
      filter(DateTime <= end_date)
  }
  
  
  ### Add flags for missing values ####
  # for loop to create flag columns
  for(j in c(2:9)) { #for loop to create new columns in data frame
    All_Inflow[c(which(is.na(All_Inflow[,j]) & (All_Inflow[,paste0("Flag_",colnames(All_Inflow[j]))]==0))), paste0("Flag_",colnames(All_Inflow[j]))]=7#s #puts in flag 7 if value not collected
  }
  
  #### Add Reservoir and Site ####
  # Get it ready for publishing and get the maintenance log ready.
  #creating columns for EDI
  All_Inflow$Reservoir <- "FCR" #creates reservoir column to match other data sets
  All_Inflow$Site <- 100  #creates site column to match other data sets
  

  # Re organize columns
  Final<-All_Inflow%>%
    select(Reservoir, Site, everything())
  
  # Save Rating curve file
  if(!is.null(output_file_rating_curve)){
  write_csv(df, output_file_rating_curve)
  }
  
  # Save file to return it if output_file = NULL
  if(is.null(output_file)){
    
    return(Final)
  }else{
    # convert DateTime
    
    Final$DateTime <- as.character(format(Final$DateTime))
    
    # save the file
    
    write_csv(Final,output_file)
    
  }
  
  print('Inflow File QAQCed!')
}
