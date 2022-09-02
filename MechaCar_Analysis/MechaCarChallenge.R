setwd("~/Desktop/DataAnalytics /MechaCar_Analysis")
library(jsonlite)
library(tidyverse)
MechaCars <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

#Find co-efficients
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = MechaCars)

# Find the p and r-squared value
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = MechaCars))

# Read in suspension coil csv
suspension <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# Create total total summary
total_summary <- suspension %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# Create lot summary
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# T-Tests
t.test(suspension$PSI, mu=1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot 1")$PSI, mu=1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot 2")$PSI, mu=1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot 3")$PSI, mu=1500)


