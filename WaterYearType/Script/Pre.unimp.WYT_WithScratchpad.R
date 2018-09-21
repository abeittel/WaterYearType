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


#5) Categorize all mean annual flow values into WYT quartile bins. Quartile values are currently writen as minimum values for a WYT bin (example: if mean annual flow is greater or EQUAL to the Q1 value and less than but NOT EQUAL to Q2 value, then that year is "below moderate").
WYT_all <- mutate(all, WYT = ifelse(Mean_Annual_Flow < Q1, "Dry",
                         ifelse(Mean_Annual_Flow >= Q1 & Mean_Annual_Flow < Q2, "Below Moderate",
                         ifelse(Mean_Annual_Flow >= Q2 & Mean_Annual_Flow < Q3, "Above Moderate", 
                         ifelse(Mean_Annual_Flow >= Q3, "Wet" , NA))))) 


#6) Join USGS gage ID to corresponding COMID
data <- list.files(fp)
USGSgage <- read.csv("Ref gages for WYT_9.20.2018.csv")
View(USGSgage)
USGSgage <- USGSgage %>%
            select(USGS_GAGE, NHDV1_COMI) %>%
            rename(COMID = NHDV1_COMI)
WYT_with.USGS.gage <- left_join(USGSgage, WYT_all, by = "COMID")
# rename new dataframe with both USGS gage ID and COMID 
WYT_USGS_all <- WYT_with.USGS.gage
View(WYT_USGS_all)


#7) Create final dataframe
#a) Select all desired columns for final dataframe 
WYT_final <- select(WYT_USGS_all, USGS_GAGE, COMID, Water_Year, Q1, Q2, Q3, WYT, Mean_Annual_Flow, POR_avg_annual_flow, Count_water_months, Count_monthlyavg)
View(WYT_final)
#b) export dataframe as csv
write.csv(WYT_final, "X:/environmental_flows/WaterYearType/Results/Modeled.Predicted.Unimpaired.15+.WYT_09.21.2018.csv")




#________________Alice's scratchpad and notes..._______________
#mutate' to make new column with yearly averages - compute new variables by group
yearly <- mutate(monthlygages, MeanAnnualFlow = mean())

montly$TIMESTEP = ymd(monthly$TIMESTEP)

annualmean <- group_by(monthlymean, Year = year(Year), Month = month(Month)) %>%
  summarise(MeanAnnualFlow = mean(MINIMUM.C.),
            meanDailyMean=mean(MEAN.C.))

for(Year in c(1950:2015)){
  #take out set of rows => extract 12 months for 1 year for 1 COMID, average => move to next year for 1C COMID... => move to 2COMID
  monthly 
  %>% group_by(Year) 
  %>% group_by(COMID, add = TRUE)
  %>% mutate(monthly, )
  
}

tidyr::spread(pollution, size, amount)

# Calcualte quartiles values (mean annual flow or mean monthly flow?) for each gage 
#gages_perc <- mutate(monthlygages, percentile = cut(Estimated.Q, breaks = c(-Inf, 6, 38, 290, Inf), labels = c ("Dry", "Below Moderate", "Above Moderate", "Wet")))

head(gages_perc)
?within

#calculate yearly averages per water year
for(COMID in monthlymean){
  annualmean <- monthlymean %>%
    group_by(Water_Year) %>%
    summarise(Mean_Annual_Flow = mean(Estimated.Q))
  return(annualmean)
}

# Assign WYTs for each gage based on above => 


# Create new column with flow quartiles based on _______ flows
gages_perc <- within(gages, quartile <- as.integer (cut(Estimated.Q, quantile(Estimated.Q, probs = 0:4/4), include.lowest = TRUE))) 
head(gages_perc)

# Create new column with categorical labels for quartiles (MONTHLY water types)
gages_WMonthT <- mutate(gages_perc, WMT = cut(quartile, breaks = c(-Inf, 1, 2, 3, 4), labels = c ("Dry", "Below Moderate", "Above Moderate", "Wet")))
head(gages_WMT)

# Create new column with YEARLY water types 
gages_WYearT <- mutate(gages_WMonthT, WYearT = cut(WMT))


# Find percentile bins for each month for all years....confused on what the values are in the dataframe....saying there are 6 years, COMID, or 6 total values under 25%....????
percentiles <- group_by(gages, Month) %>%
  summarise(`25%`=quantile(Estimated.Q, probs=0.25),
            `50%`=quantile(Estimated.Q, probs=0.5),
            `75%`=quantile(Estimated.Q, probs=0.75),
            avg=mean(Estimated.Q),
            n=n())
View(percentiles)

#january
#gages_perc <- mutate(gages, percentile = factor(1*(Estimated.Q < 7) 7 < Estimated.Q < 39, 39 < Estimated.Q < 291, 291 < Estimated.Q), lables = c("25%", "50%", "75%", "75+%")))

#YES! Assigns catagorical year types for all january months for all years...now to figure out how to do for all months...
gages_perc <- mutate(gages, percentile = cut(Estimated.Q, breaks = c(-Inf, 6, 38, 290, Inf), labels = c ("Dry", "Below Moderate", "Above Moderate", "Wet")))
head(gages_perc)

head(filter(gages_perc, Month==1))

#maybe a loop will work?...no bc breaks are determined by jan. quartiles....
for (Month in c(1:12)){
  gages_perc <- mutate(gages, percentile = cut(Estimated.Q, breaks = c(-Inf, 6, 38, 290, Inf), labels = c ("Dry", "Below Moderate", "Above Moderate", "Wet")))
}

#gages_four <- gages %>% mutate(quintile = ntile(Estimated.Q, 4))
#head(gages_four)


# Steps
#   1. read.csv and make a data frame
#   2. Average all monthly means for a gage to get the percentile cut offs/WYT bins
#   3. Average each year individually per gage and assign WYT

