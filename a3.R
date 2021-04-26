setwd("C:/Users/DELL/Desktop")

rm(list = ls())

library(dplyr)
library(AER)
library(plm)

## Exercise 1
population <- read.csv("population.csv")
crime_long <- read.csv("crime_long.csv")
officers <- read.csv("officers.csv")

## Exercise 2

# Exercise 2-1. Calculate total crime per month and plot the time series of crime

crime_group <-group_by(crime_long, crime_month)
crime_groupbymonth <- summarise(crime_group, crimes = sum(crimes))
mean(crime_groupbymonth$crimes)
plot(x = as.Date(crime_groupbymonth$crime_month),
     y = crime_groupbymonth$crimes,
     main = "Time Series of Crime",
     xlab = "time",
     ylab = "number of crimes")

# Exercise 2-2. Merge the two datasets by districts-units and period

data1 <- merge(x=crime_long, y=population, by.x = c("crime_month","district"), 
               by.y = c("month","district"), all.y =TRUE)

# Exercise 2-3. Construct a panel data of unit over time

crime_group1 <- group_by(crime_long, crime_month, district)
crime_groupbymonthunit <- summarise(crime_group1, districtcrimes = sum(crimes))
data2 <- merge(x = data1, y = crime_groupbymonthunit, by =c("crime_month","district"), all.x = TRUE)

data_violent <- filter(data2,crime_type=="violent")
violent_group <- group_by(data_violent, crime_month, district)
data_violent <- summarise(violent_group, violentcrimes = sum(crimes))

data_property <- filter(data2,crime_type=="property")
property_group <- group_by(data_property, crime_month, district)
data_property <- summarise(property_group, propertycrimes = sum(crimes))

data3 <- merge(x = data2, y = data_violent, by = c("crime_month","district"), all.x = TRUE)
data3 <- merge(x = data3, y = data_property, by = c("crime_month","district"), all.x = TRUE)

data3$crimeper <- data3$districtcrimes / data3$tot_pop
data3$violentper <- data3$violentcrimes / data3$tot_pop
data3$propertyper <- data3$propertycrimes / data3$tot_pop
data3$blackshare <- data3$tot_black / data3$tot_pop
data3$hispshare <- data3$tot_hisp / data3$tot_pop
data3$whiteshare <- data3$tot_white / data3$tot_pop

data4 <- data3
data4 <- select(data4, -c(3:9))
data4 <- unique(data4)

## Exercise 3

data5 <- merge(x = officers, y = data4, by.x = c("month","unit"), 
               by.y = c("crime_month","district"), all.x = TRUE)

model1 <- lm(arrest~tenure+districtcrimes+p50_inc+blackshare+hispshare+whiteshare-1, data = data5)
summary(model1)

## Exercise 4

model2 <- lm(arrest~tenure+districtcrimes+p50_inc+blackshare+hispshare+
               whiteshare+factor(unit)+factor(month)-1, data = data5)
summary(model2)

## Exercise 5

# 5-1. Implement a within, between, and first difference estimator

# Within estimator

data5$unit <- as.factor(data5$unit)
modelwithin <- plm(arrest~tenure+districtcrimes+p50_inc+blackshare+hispshare+whiteshare+unit-1,
                   data = data5,
                   index = c("month","NUID"),
                   model = "within",
                   effect = "individual")

# Between estimator

modelbetween <- plm(arrest~tenure+districtcrimes+p50_inc+blackshare+hispshare+whiteshare+unit-1,
                    data = data5,
                    index = c("month","NUID"),
                    model = "between")

# First difference estimator

modelfd <- plm(arrest~tenure+districtcrimes+p50_inc+blackshare+hispshare+whiteshare+unit-1,
               data = data5,
               index = c("month","NUID"),
               model = "fd")

# Comparison

modelwithin$coefficients[1]
modelbetween$coefficients[1]
modelfd$coefficients[1]


