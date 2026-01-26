#install.packages("EDIutils")
#install.packages("xml2")
#install.packages("here")
#install.packages('suncalc')
library(tidyverse)
library(EDIutils)
library(xml2)
library(lubridate)
library(suncalc)

home_directory <- here::here()
setwd(home_directory)

### pull in QAQC function directly from EDI -- keep for now, but will need to update the function
#source('https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.7&entityid=03b6589579059fd8184a56a5831138ec')

## pull in QAQC function from script stored on Github -- use for now, but want to use EDI pull eventually
source('R/edi_qaqc_function.R')

## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.389.10'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)

day_of_run <- Sys.Date() + lubridate::days(1)

## assign data files 
met_data <- c('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data/FCRmet.csv', 'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/current_files/FCRMet_L1.csv')
#met_data <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv'
met_infrared_data <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCR_Met_Infrad_DOY_Avg_2018.csv'
#maintenance_file <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/MET_MaintenanceLog.txt'
maintenance_file <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/MET_maintenancelog_new.csv'
outfile <-'FCRmet_L1.csv'

## run QAQC on the data within github
qaqc_fcrmet(data_file = met_data, 
            maintenance_file = maintenance_file, 
            met_infrad = met_infrared_data, 
            output_file = outfile, 
            start_date = last_edi_date,  
            end_date = day_of_run,
            notes = FALSE)

#wq_qaqc <- read_csv('FCRmet_L1.csv')

## convert all flag columns from numeric to factor data type -- this is also done inside of the function. Needs to be called at FLARE run time in the future, unless the flag columns are changed
# wq_qaqc <- wq_qaqc %>%
#   mutate(across(starts_with("Flag"),
#                 ~ as.factor(as.character(.))))
