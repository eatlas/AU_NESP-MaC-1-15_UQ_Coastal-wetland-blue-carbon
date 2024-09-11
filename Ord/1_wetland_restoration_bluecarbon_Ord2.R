# Reads in and manipulates restoration areas and calculates blue carbon - preliminary estimates
# Valerie Hagger 22/11/2021
rm(list = ls())
setwd("R:/NESP115-Q4418/R_working_directory/Ord") #sets the working directory

# load packages
library(tidyverse)
library(readxl)

### Restoration Opportunity ###

#calculate Ord potential restoration area with 5 km aggregation of sites
rsites.Ord.ag5 <- read.csv("R:/NESP115-Q4418/R_working_directory/Ord/Rsite_area_ha.csv", header=T)
colnames(rsites.Ord.ag5)
range(rsites.Ord.ag5$Area..ha.)
length(rsites.Ord.ag5$Area..ha.)
dim(subset(rsites.Ord.ag5, rsites.Ord.ag5$Area..ha.<100))
dim(subset(rsites.Ord.ag5, rsites.Ord.ag5$Area..ha.>=100))
rsites.Ord.ag.sum <- sum(rsites.Ord.ag5$Area..ha.)


### Avoided emissions from avoided soil organic carbon losses associated with reduced grazing ###
# avoided emission of 0.3 CO2-e ha-1 year-1
rsites.Ord.ag.sum*0.3

#read in scaled cobenefits
rsites.ES <- read_excel("Scaled_cobenefits.xlsx", sheet="Sheet1")
colnames(rsites.ES)
#join mangrove (coastal protection benefit)
mang.area <- read.csv("Mangrove area rsite.csv")
colnames(mang.area)
rsites.ES2 <- full_join(rsites.ES, mang.area, by="OBJECTID")
colnames(rsites.ES2)
rsites.ES2$Mangrove_area_ha
rsites.ES2$Mangrove_area_ha[is.na(rsites.ES2$Mangrove_area_ha)] <- 0
rsites.ES2$Mang_area_ha_scale <- (rsites.ES2$Mangrove_area_ha/max(rsites.ES2$Mangrove_area_ha))*100
rsites.ES2$FLOOD <- rsites.ES2$Mang_area_ha_scale

rsites.ES2$ES_equal <- (rsites.ES2$BIOD + rsites.ES2$FISH + rsites.ES2$FLOOD)/3

hist(rsites.ES2$ES_equal)
mean <- mean(rsites.ES2$ES_equal)
EShigh <- subset(rsites.ES2, rsites.ES2$ES_equal>mean)
sum(EShigh$`Area (ha)`)

