#install.packages("EDIutils")
#install.packages("xml2")
#install.packages("here")
library(tidyverse)
library(EDIutils)
library(xml2)
library(lubridate)

#home_directory <- here::here()
home_directory <- getwd()
setwd(home_directory)

### pull in QAQC function directly from EDI -- keep for now, but will need to update the function
#source('https://portal.edirepository.org/nis/dataviewer?packageid=edi.271.7&entityid=1e853e00adc8e1f986a3a4b1586a231f')

## pull in QAQC function from script stored on Github -- use for now, but want to use EDI pull eventually
source('R/qaqc_function.R')

## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
package_ID <- 'edi.202.13'
eml <- read_metadata(package_ID)
date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)

day_of_run <- Sys.Date() + lubridate::days(1)

## assign data files 
wq_data <- c('https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data/FCRweir.csv',
             'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/current_files/WeirData_L1.csv'),
pressure <- 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_inflow/WVWA_weirInflow_L1.csv'
maintenance_url <- 'Weir_MaintenanceLog.csv'
gauge_reading <- 'Inflow_Gauge_Height_at_Weir.csv'
outfile <-'FCRWeir_L1.csv'
output_file_rating_curve <- 'Rating_Curve_Calculations.csv'

## run QAQC on the data within github
qaqc_fcrweir(VT_data_file = wq_data, 
             WVWA_data_file = pressure, 
             maintenance_file = maintenance_url, 
             Staff_gauge_readings = gauge_reading, 
             output_file = outfile, 
             output_file_rating_curve = output_file_rating_curve,
             start_date = last_edi_date, 
             end_date = day_of_run)

#wq_qaqc <- read_csv('fcre-waterquality_L1.csv')

## convert all flag columns from numeric to factor data type -- this is also done inside of the function. Needs to be called at FLARE run time in the future, unless the flag columns are changed
# wq_qaqc <- wq_qaqc %>%
#   mutate(across(starts_with("Flag"),
#                 ~ as.factor(as.character(.))))
