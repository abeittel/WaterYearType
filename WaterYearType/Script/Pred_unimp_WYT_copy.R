library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(lubridate)

fp <- "Users/AliceBeittel/Documents/Academia/4th Year/CWS/R_Practice"
setwd(fp)
data <- list.files(fp)
gages <- read.csv("COMID_Monthly_Natural_Predict_91.csv")
View(gages)

?filter
#CALCULATE YEARLY AVERAGES:
#A) 'select' columns COMID, YEAR, MONTH, ESTIMATED.Q 
str(gages)
names(gages)
monthly <- select(gages, COMID, Year, Month, Estimated.Q)
View(monthly)
#B) 'mutate' to make new column with yearly averages 
head(monthly)
yearly <- mutate(monthly, MeanAnnualFlow = mean())

montly$TIMESTEP = ymd(monthly$TIMESTEP)

monthly %>% 
  group_by(Year = year(Year), Month = month(Month)) %>%
  summarise(MeanAnnualFlow = mean(MINIMUM.C.),
            meanDailyMean=mean(MEAN.C.))
for(Year in c(1950:2015)){
  #take out set of rows => extract 12 months for 1 year for 1 COMID => move to next year for 1C COMID... => move to 2COMID
  monthly 
  %>% group_by(Year) 
  %>% group_by(COMID, add = TRUE)
  %>% mutate(monthly, )
  
}

tidyr::spread(pollution, size, amount)

# Calcualte quartiles values (mean annual flow or mean monthly flow?) for each gage 
for (Month in c(1:12)){
  gages_perc <- mutate(gages, percentile = cut(Estimated.Q, breaks = c(-Inf, 6, 38, 290, Inf), labels = c ("Dry", "Below Moderate", "Above Moderate", "Wet")))
}

?within
# Assign WYTs for each gage based on above => 


# Create new column with flow quartiles based on _______ flows
gages_perc <- within(gages, quartile <- as.integer (cut(Estimated.Q, quantile(Estimated.Q, probs = 0:4/4), include.lowest = TRUE))) 
head(gages_perc)

# Create new column with categorical labels for quartiles (MONTHLY water types)
gages_WMonthT <- mutate(gages_perc, WMT = cut(quartile, breaks = c(-Inf, 1, 2, 3, 4), labels = c ("Dry", "Below Moderate", "Above Moderate", "Wet")))
head(gages_WMT)

# Create new column with YEARLY water types 
gages_WYearT <- mutate(gages_WMonthT, WYearT = cut(WMT))



#workspace

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
#   2. Use estimated.Q to calculate the percentile cut offs
#   2. create empty data frame -> extract monthly mean predicted flow (estimated.Q) and calculate yearly mean predicted flow for each COMID 

