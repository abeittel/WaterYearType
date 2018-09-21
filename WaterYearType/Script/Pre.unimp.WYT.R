# Code to determine modeled predicted unimpaired water year types for 173 reference gages of California 
# Author: Alice Beittel

#install.packages("raster")
#install.packages("sp")
#install.packages("rgdal")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("lubridate")


library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(lubridate)
#https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf 

#Setup 
fp <- "X:/environmental_flows/WaterYearType/Data"
setwd(fp)
data <- list.files(fp)
gages <- read.csv("COMID_Monthly_Natural_Predict_173.csv") 
# => dataset must contain no naturalized or modeled data and period of record (POR) should be > 15yrs and within 1950-2015
View(gages)


#1) Convert calendar years/months to water years/months:
      #12-month period starting October 1, for any given year through September 30, of the following year - The water year is   designated by the calendar year in which it ends and which includes 9 of the 12 months. Thus, the year ending September 30, 1999 is called the "1999" water year.

#a) 'select' columns needed (COMID, YEAR, MONTH, ESTIMATED.Q)
str(gages)
names(gages)
monthlymean <- select(gages, COMID, Year, Month, Estimated.Q)
View(monthlymean)

#b) 'rename' current year/month columns to "calendar" year/month
monthlymean <- rename(monthlymean, Calendar_Year = Year) %>%
               rename(Calendar_Month = Month)
#c) make functions to do the conversion
# => function for converting calendar year to water year
wtr_yr <- function(year, month) {
  wtr_yr <- ifelse(month >= 10, year+1, year)
  
  return(wtr_yr)
}
# => function for converting calendar month to water month
wtr_mo <- function(month) {
  wtr_mo <- ifelse(month == 10:12, month-9, month+3) 
  return(wtr_mo)
}

#d) execute functions and create new columns for water year/month
monthlymean <- monthlymean %>%
               mutate(Water_Year = wtr_yr(monthlymean$Calendar_Year, monthlymean$Calendar_Month)) %>%
               mutate(Water_Month = wtr_mo(monthlymean$Calendar_Month))

View(monthlymean)


#2) Calculate quartiles for each COMID => quartiles are based on the entire period of record for one COMID(gage) => Each gage will have unique quartiles but the same quartiles will be used for each year within a gage's POR to determine the WYT.

            # group all the monthly avg flows (Estimated.Q) for each COMID
quartiles <- group_by(monthlymean, COMID) %>%
            # make quartiles for each COMID for entire POR => Q1=25%, Q2=50%, Q3=75%
             summarise(`Q1`=quantile(Estimated.Q, probs=0.25),
                       `Q2`=quantile(Estimated.Q, probs=0.5),
                       `Q3`=quantile(Estimated.Q, probs=0.75),
                       # check...median should match Q2  
                        median=median(Estimated.Q),
                       # average of all monthly means for entire POR for each COMID(gage)
                        POR_avg_annual_flow=mean(Estimated.Q),
                       # number of average monthly values used to calculate the quartiles, should be 792
                        Count_monthlyavg=n())
View(quartiles)


#3) Calculate the mean annual flow for each COMID(gage) for each water year (example: average of all monthly flows for water year 1968 for one reference gage).

annualmean <- group_by(monthlymean, COMID) %>%
              group_by(Water_Year, add = TRUE) %>%
              summarise(Mean_Annual_Flow=mean(Estimated.Q),
                        # number of water months used to calculate the mean annual flow for ONE year - because data set started with Jan, first water year average will only have 9 months of data instead of 12. 
                        Count_water_months=n())
View(annualmean)


#4) Join the 'annualmean' and 'quartiles' by COMID(gage) to get one table.
all <- left_join(annualmean, quartiles, by = "COMID")
View(all)


#5) Categorize all mean annual flow values into WYT quartile bins. Quartile values are currently written as minimum values for a WYT bin (example: if mean annual flow is greater or EQUAL to the Q1 value and less than but NOT EQUAL to Q2 value, then that year is "below moderate").
WYT_all <- mutate(all, WYT = ifelse(Mean_Annual_Flow < Q1, "Dry",
                         ifelse(Mean_Annual_Flow >= Q1 & Mean_Annual_Flow < Q2, "Below Moderate",
                         ifelse(Mean_Annual_Flow >= Q2 & Mean_Annual_Flow < Q3, "Above Moderate", 
                         ifelse(Mean_Annual_Flow >= Q3, "Wet" , NA))))) 


#6) Join USGS gage ID to corresponding COMID
data <- list.files(fp)
USGSgage <- read.csv("Ref gages for WYT_9.20.2018.csv")
View(USGSgage)
USGSgage <- USGSgage %>%
            select(USGS_GAGE, NHDV1_COMI, STATION_NA) %>%
            rename(COMID = NHDV1_COMI)
WYT_with.USGS.gage <- left_join(USGSgage, WYT_all, by = "COMID")
# rename new dataframe with both USGS gage ID and COMID 
WYT_USGS_all <- WYT_with.USGS.gage
View(WYT_USGS_all)


#7) Create final dataframe
#a) Select all desired columns for final data frame 
WYT_final <- select(WYT_USGS_all, USGS_GAGE, COMID, STATION_NA, Water_Year, Q1, Q2, Q3, Mean_Annual_Flow, WYT, POR_avg_annual_flow, Count_water_months, Count_monthlyavg)
View(WYT_final)
#b) export dataframe as csv
write.csv(WYT_final, "X:/environmental_flows/WaterYearType/Results/Modeled.Predicted.Unimpaired.15+.WYT_09.21.2018.csv")


