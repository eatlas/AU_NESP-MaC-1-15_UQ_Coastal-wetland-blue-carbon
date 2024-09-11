# Economic analysis
# Phoebe Stewart-Sinclair and Valerie Hagger 16/02/2024
rm(list = ls())
setwd("R:/NESP115-Q4418/R_working_directory/Peel") #sets the working directory

# load packages
library(tidyverse)
library(readxl)

# ---S2--- FARM GROSS MARGINS

# Add GM for cattle from Farm Gross Margin and Enterprise Planning Guide 2022 - AUD/ha
#1	$593.12 beef cattle - high rainfall
#2	$83.99 beef trading - high rainfall
#stocking density 10 DSE/ha for each

#Dont need to change the currency as it's in 2022 AUD
gm_prod_groups <- read.csv("GM_prodgroups_PHSW.csv", header=T)
colnames(gm_prod_groups)

# add stocking densities for each site
#Don't need this, it's 10DSE per ha. 

# identify GLMs for each site
#don't need this as we assume they are all the same.
#import rsites with size (restor_ha)
rsites <- read.csv("PHSW_grazrsites_ha.csv", header=T)
colnames(rsites)

#calculate farm gross margin per year per land type of each site
rsites$FGM1 <- with(rsites, restor_ha*593.12) #for beef cattle
rsites$FGM2 <- with(rsites, restor_ha*83.99) #for beef trading
head(rsites) #check calc

#sum farm gross margin per site
#this is same as above
# select and spread data from long to wide
colnames(rsites)

# assign land uses based on productivity and to account for different income
#all grazing so don't need this step 

# ---S4--- RESTORATION COSTS
maint_AUD_5 <- 750
wetland_lower <- 5000
wetland_upper <- 275130
maint_AUD_25 <- 0

# Read in and manipulate mangrove cost data
#No mangroves in this but use these for supratidal
# escalate to Dec 2022 from Dec 2010 using CPI (Index groups, All groups, Brisbane)
CPI_Dec10_Dec22 <- (132.1-97.4)/97.4

Bayrak.mangrove <- read.csv("Bayraktarov_2016_mangroves_section.csv", header=T)
colnames(Bayrak.mangrove)
library(tidyverse)
Bayrak.mangrove.costs <- Bayrak.mangrove %>% 
  dplyr::select("Specific.technique", "Country", economy.type="Economy.type..developed.or.developing.", restoration.cost="Restoration.cost.in...US....ha...2010.base..CPI")
head(Bayrak.mangrove.costs)
xc.AUD.USD.2010<-1.09016
Bayrak.mangrove.costs$restoration.cost.AUD<-Bayrak.mangrove.costs$restoration.cost*xc.AUD.USD.2010 #convert costs to AUD in 2010 exchange rate from Penn Table
Bayrak.mangrove.costs$restoration.cost.AUD21<-Bayrak.mangrove.costs$restoration.cost.AUD*(1+CPI_Dec10_Dec22) #convert costs DEc 19 to Dec 2022 using all groups CPI Brisbane perc change (5.30%)

mangrove.costs.developed<-with(Bayrak.mangrove.costs, tapply(restoration.cost, economy.type, median, na.rm=T)) 
mangrove.costs.technique<-with(Bayrak.mangrove.costs, tapply(restoration.cost, Specific.technique, median, na.rm=T)) %>% as.matrix
mangrove.costs.tech.devel.med<-with(Bayrak.mangrove.costs, tapply(restoration.cost, list(Specific.technique, economy.type), median, na.rm=T)) %>% as.matrix
mangrove.costs.tech.devel.mean<-with(Bayrak.mangrove.costs, tapply(restoration.cost, list(Specific.technique, economy.type), mean, na.rm=T)) %>% as.matrix

mangrove.costs.tech.devel.med.Aus<-with(Bayrak.mangrove.costs, tapply(restoration.cost.AUD21, list(Specific.technique, economy.type), median, na.rm=T)) %>% as.matrix
mangrove.cost.hydro.AUD.ha.med<-mangrove.costs.tech.devel.med.Aus[5,1]
mangrove.cost.hydro.planting.AUD.ha.med<-mangrove.costs.tech.devel.med.Aus[6,1]

mangrove.costs.tech.count.med.Aus<-with(Bayrak.mangrove.costs, tapply(restoration.cost.AUD21, list(Specific.technique, Country), median, na.rm=T)) %>% as.matrix
mangrove.costs.tech.count.med.Aus<-with(Bayrak.mangrove.costs, tapply(restoration.cost.AUD21, list(Country, economy.type), median, na.rm=T)) %>% as.matrix


### "habitat conversion" applys to restoration of disused shrimp and aquaculture ponds, 
#"Hydrological restoration - facilitation of a natural recovery" applys to this project

# mean and median of mangrove restoration costs in Australia - note only costs for planting exist
mangrove.costs.Aus<-subset(Bayrak.mangrove.costs, Bayrak.mangrove.costs$Country == "Australia") #subset of Australia only data
mangrove.costs.Aus.tech.med<-with(mangrove.costs.Aus, tapply(restoration.cost.AUD, Specific.technique, median, na.rm=T)) %>% as.matrix
median(mangrove.costs.Aus$restoration.cost.AUD, na.rm=T)
#4911.25
mean(mangrove.costs.Aus$restoration.cost.AUD, na.rm=T)
#46649.91

# costs for saltmarsh restoration
Bayrak.saltmarsh <- read.csv("Bayraktarov_2016_saltmarsh_section.csv", header=T)
colnames(Bayrak.saltmarsh)

Bayrak.saltmarsh.costs <- Bayrak.saltmarsh %>% 
  dplyr::select("Specific.technique", "Country", economy.type="Economy.type..developed.or.developing.", restoration.cost="Restoration.cost.in...US....ha...2010.base..CPI")
head(Bayrak.saltmarsh.costs)
Bayrak.saltmarsh.costs$restoration.cost.AUD<-Bayrak.saltmarsh.costs$restoration.cost*xc.AUD.USD.2010 #convert costs to AUD in 2010 exchange rate from Penn Table
Bayrak.saltmarsh.costs$restoration.cost.AUD21<-Bayrak.saltmarsh.costs$restoration.cost.AUD*(1+CPI_Dec10_Dec22) #convert to 2022 value

saltmarsh.costs.developed<-with(Bayrak.saltmarsh.costs, tapply(restoration.cost, economy.type, median, na.rm=T)) %>% as.matrix
saltmarsh.costs.technique<-with(Bayrak.saltmarsh.costs, tapply(restoration.cost, Specific.technique, median, na.rm=T)) %>% as.matrix
saltmarsh.costs.tech.devel<-with(Bayrak.saltmarsh.costs, tapply(restoration.cost, list(Specific.technique, economy.type), median, na.rm=T)) %>% as.matrix

table(Bayrak.saltmarsh.costs$economy.type) #only saltmarsh costs in developed nations

saltmarsh.costs.tech.devel.med.Aus<-with(Bayrak.saltmarsh.costs, tapply(restoration.cost.AUD21, list(Specific.technique, economy.type), median, na.rm=T)) %>% as.matrix
saltmarsh.costs.tech.count.med.Aus<-with(Bayrak.saltmarsh.costs, tapply(restoration.cost.AUD21, list(Specific.technique, Country), median, na.rm=T)) %>% as.matrix

#combine hydrological restoration and managed retreat and take median
saltmarsh.cost.hydro<-subset(Bayrak.saltmarsh.costs, Bayrak.saltmarsh.costs$Specific.technique == "Hydrological restoration" |  Bayrak.saltmarsh.costs$Specific.technique == "Hydrological restoration, managed retreat") #subset hydrological restoration data
saltmarsh.cost.hydro.AUD.ha.med<-median(saltmarsh.cost.hydro$restoration.cost.AUD21, na.rm=T)
saltmarsh.cost.hydro.AUD.ha.mean<-mean(saltmarsh.cost.hydro$restoration.cost.AUD21, na.rm=T)

#combine hydrological restoration planting saltmarsh and clearing exotic veg and take median
saltmarsh.cost.hydro.planting<-subset(Bayrak.saltmarsh.costs, Bayrak.saltmarsh.costs$Specific.technique == "Hydrological restoration, clearing exotic vegetation, planting saltmarsh" |  Bayrak.saltmarsh.costs$Specific.technique == "Hydrological restoration, planting saltmarsh") 
saltmarsh.cost.hydro.planting.AUD.ha.med<-median(saltmarsh.cost.hydro.planting$restoration.cost.AUD21, na.rm=T)
saltmarsh.cost.hydro.planting.AUD.ha.mean<-mean(saltmarsh.cost.hydro.planting$restoration.cost.AUD21, na.rm=T)

mid.cost.hydro.AUD.ha.med<-(saltmarsh.cost.hydro.AUD.ha.med+mangrove.cost.hydro.AUD.ha.med)/2
mid.cost.hydro.planting.AUD.ha.med <- (saltmarsh.cost.hydro.planting.AUD.ha.med+mangrove.cost.hydro.planting.AUD.ha.med)/2


# ---S5--- ECONOMIC ANALYSIS 
# Read in dataframes with carbon benefit
CBA <- read.csv("rest.sites.abatement.pa.25years.csv", header=T)
CBA.upper <- read.csv("rest.sites.abatement.pa.25years.upper.csv", header=T)
CBA.lower <- read.csv("rest.sites.abatement.pa.25years.lower.csv", header=T)
CBA.100 <- read.csv("rest.sites.abatement.pa.100yrs.csv", header=T)

#add areas
#we haven't subsetted by basins in this PHSW
#just use rsites with areas
colnames(rsites)
CBA <- full_join(rsites, CBA, by="FID_site")
CBA.upper <- full_join(rsites, CBA.upper, by="FID_site")
CBA.lower <- full_join(rsites, CBA.lower, by="FID_site")
CBA.100 <- full_join(rsites, CBA.100, by="FID_site")

#add FGMS
#already in this file
list(CBA$FGM1) 
list(CBA$FGM2)

#convert NAs to 0
CBA$FGM1[is.na(CBA$FGM1)] <- 0
CBA$FGM2[is.na(CBA$FGM2)] <- 0

#---INCORPORATE ECONOMIES OF SCALE---
#install.packages("GLDEX")
#library(GLDEX)
range(CBA$restor_ha) 
#1.00 47.89
#which.na(CBA$restor_ha)
#integer(0)

scale <- read.csv("economies_scale.csv", header=T)
scale
rate1 <- 1
rate5 <- scale[2,3]
rate10 <- scale[3,3]
rate25 <- scale[4,3]
rate50 <- scale[5,3]
rate100 <- scale[6,3]

# add new column for matching discount rates to size classes
#floor(x) rounds to the nearest integer that's smaller than x, so floor(123.45) becomes 123 and floor(-123.45) becomes -124
CBA$rate_scale <- 0
CBA$rate_scale <- ifelse(CBA$restor_ha>=5,rate5,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$restor_ha>=10,rate10,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$restor_ha>=25,rate25,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$restor_ha>=50,rate50,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$restor_ha>=100,rate100,CBA$rate_scale)
head(CBA$rate_scale)

#calcuate scaled maintenance costs
CBA$maint_cost_scale <- (maint_AUD_5+(maint_AUD_5*CBA$rate_scale))*CBA$restor_ha
list(CBA$rate_scale) #check
list(CBA$restor_ha)
head(CBA$maint_cost_scale)

#calculate scaled restoration costs
CBA$rest_low_cost_scale <- (saltmarsh.cost.hydro.AUD.ha.med+(saltmarsh.cost.hydro.AUD.ha.med*CBA$rate_scale))*CBA$restor_ha
CBA$rest_high_cost_scale <- (mangrove.cost.hydro.AUD.ha.med+(mangrove.cost.hydro.AUD.ha.med*CBA$rate_scale))*CBA$restor_ha
head(CBA$rate_scale)
head(CBA$restor_ha)
head(CBA$rest_low_cost_scale) #check
head(CBA$rest_high_cost_scale) 

# calculate financial benefit of removals and avoided emissions
#CP <- (16.94+15.99+15.74)/3 # average weighted carbon price (AUD Mg CO2e) over three last auction results - Oct 2021, April 2021 and Sept 2020 http://www.cleanenergyregulator.gov.au/ERF/auctions-results
CP <- 57 #the peak ACCU spot price of AU$57 per Mg CO2-e  on 24 January 2022
CP.high <- 132 #higher international carbon unit (European Union Allowances) spot price of AU $132 on 31 December 2022 

# Logged scenario
# Annuity GM with average CP across 25 years

# mutate to AUD given carbon price 
funCP <- function(x) (x*CP)
CBA.CPlow <- CBA %>% mutate(across(starts_with("abate_yr"), funCP, .names = "{.col}_AUD"))
#CBA.CPlow <- CBA %>% as.tibble % mutate(across(starts_with("abate_yr"), funs(.*CP), .names = "{.col}_AUD"))
colnames(CBA.CPlow) #check new vars, has replaced existing ones
head(CBA.CPlow$abate_yr1)
head(CBA.CPlow$abate_yr1_AUD)

funCPhigh <- function(x) (x*CP.high)
CBA.CPhigh <- CBA %>% mutate(across(starts_with("abate_yr"), funCPhigh, .names = "{.col}_AUD"))
#CBA.CPhigh <- CBA %>% as.tibble %>% mutate(across(starts_with("abate_yr"), funs(.*CP.high), .names = "{.col}_AUD"))
colnames(CBA.CPhigh) #check new vars
head(CBA.CPhigh$abate_yr1)
head(CBA.CPhigh$abate_yr1_AUD)

#mutate to minus off maintenance cost for first five years
funMaint <- function(x) (x-CBA.CPlow$maint_cost_scale)
CBA.CPlow.maint <- CBA.CPlow %>% mutate(across(ends_with("AUD"), funMaint, .names = "{.col}_maint"))
                                               #funs(.-CBA.CPlow$maint_cost_scale)
colnames(CBA.CPlow.maint) #check new vars
head(CBA.CPlow.maint$abate_yr1_AUD)
head(CBA.CPlow.maint$abate_yr1_AUD_maint)
#negative numbers

funMaint2 <- function(x) (x-CBA.CPhigh$maint_cost_scale)
CBA.CPhigh.maint <- CBA.CPhigh %>% mutate(across(ends_with("AUD"), funMaint2, .names = "{.col}_maint"))
                                          #funs(.-CBA.CPhigh$maint_cost_scale) 
colnames(CBA.CPhigh.maint) #check new vars
head(CBA.CPhigh.maint$abate_yr1_AUD)
head(CBA.CPhigh.maint$abate_yr1_AUD_maint)
#negative numbers

# mutate to minus off FGM (class 1 and class 2) #have two farm practices
#minus FGM1
#minus FGM2
#so far these steps only done with FGM1 (default) not the lower FGM2
funFGM1a <- function(x) (x-CBA.CPlow.maint$FGM1)
CBA.CPlow.maint.FGM1 <- CBA.CPlow.maint %>% mutate(across(contains("AUD"), funFGM1a, .names = "{.col}_FGM1"))
  #funs(.-CBA.CPlow.maint$FGM_yr)
colnames(CBA.CPlow.maint.FGM1)
head(CBA.CPlow.maint.FGM1$abate_yr1_AUD)
head(CBA.CPlow.maint.FGM1$abate_yr1_AUD_FGM1)
#negative numbers
#has calculated abatement AUD - FGM1 for each year with low carbon price 

funFGM1b <- function(x) (x-CBA.CPhigh.maint$FGM1)
CBA.CPhigh.maint.FGM1 <- CBA.CPhigh.maint %>% mutate(across(contains("AUD"),funFGM1b, .names = "{.col}_FGM1"))
  #funs(.-CBA.CPhigh.maint$FGM_yr)
colnames(CBA.CPhigh.maint.FGM1)
head(CBA.CPhigh.maint.FGM1$abate_yr1_AUD)
head(CBA.CPhigh.maint.FGM1$abate_yr1_AUD_FGM1)
#negative numbers
#has calculated abatement AUD - FGM1 for each year with high carbon price

funFGM2a <- function(x) (x-CBA.CPlow.maint$FGM2)
CBA.CPlow.maint.FGM2 <- CBA.CPlow.maint %>% mutate(across(contains("AUD"), funFGM2a, .names = "{.col}_FGM2"))
colnames(CBA.CPlow.maint.FGM2)
head(CBA.CPlow.maint.FGM2$abate_yr1_AUD)
head(CBA.CPlow.maint.FGM2$abate_yr1_AUD_FGM2)
#negative numbers
#has calculated abatement AUD - FGM2 for each year with low carbon price 

funFGM2b <- function(x) (x-CBA.CPhigh.maint$FGM2)
CBA.CPhigh.maint.FGM2 <- CBA.CPhigh.maint %>% mutate(across(contains("AUD"),funFGM2b, .names = "{.col}_FGM2"))
colnames(CBA.CPhigh.maint.FGM2)
head(CBA.CPhigh.maint.FGM2$abate_yr1_AUD)
head(CBA.CPhigh.maint.FGM2$abate_yr1_AUD_FGM2)
#negative numbers
#has calculated abatement AUD - FGM2 for each year with high carbon price

# select columns needed
CBA.CPlow.maint.FGM1.df <- CBA.CPlow.maint.FGM1 %>% dplyr::select("abate_yr1_AUD_maint_FGM1","abate_yr2_AUD_maint_FGM1",
   "abate_yr3_AUD_maint_FGM1","abate_yr4_AUD_maint_FGM1","abate_yr5_AUD_maint_FGM1","abate_yr6_AUD_FGM1","abate_yr7_AUD_FGM1","abate_yr8_AUD_FGM1","abate_yr9_AUD_FGM1","abate_yr10_AUD_FGM1",     
   "abate_yr11_AUD_FGM1","abate_yr12_AUD_FGM1","abate_yr13_AUD_FGM1","abate_yr14_AUD_FGM1","abate_yr15_AUD_FGM1","abate_yr16_AUD_FGM1","abate_yr17_AUD_FGM1","abate_yr18_AUD_FGM1",     
   "abate_yr19_AUD_FGM1","abate_yr20_AUD_FGM1","abate_yr21_AUD_FGM1","abate_yr22_AUD_FGM1","abate_yr23_AUD_FGM1","abate_yr24_AUD_FGM1","abate_yr25_AUD_FGM1") %>% as.data.frame
CBA.CPlow.maint.FGM2.df <- CBA.CPlow.maint.FGM2 %>% dplyr::select("abate_yr1_AUD_maint_FGM2","abate_yr2_AUD_maint_FGM2",
   "abate_yr3_AUD_maint_FGM2","abate_yr4_AUD_maint_FGM2","abate_yr5_AUD_maint_FGM2","abate_yr6_AUD_FGM2","abate_yr7_AUD_FGM2","abate_yr8_AUD_FGM2","abate_yr9_AUD_FGM2","abate_yr10_AUD_FGM2",     
   "abate_yr11_AUD_FGM2","abate_yr12_AUD_FGM2","abate_yr13_AUD_FGM2","abate_yr14_AUD_FGM2","abate_yr15_AUD_FGM2","abate_yr16_AUD_FGM2","abate_yr17_AUD_FGM2","abate_yr18_AUD_FGM2",     
   "abate_yr19_AUD_FGM2","abate_yr20_AUD_FGM2","abate_yr21_AUD_FGM2","abate_yr22_AUD_FGM2","abate_yr23_AUD_FGM2","abate_yr24_AUD_FGM2","abate_yr25_AUD_FGM2") %>% as.data.frame
CBA.CPhigh.maint.FGM1.df <- CBA.CPhigh.maint.FGM1 %>% dplyr::select("abate_yr1_AUD_maint_FGM1","abate_yr2_AUD_maint_FGM1",
                                                                  "abate_yr3_AUD_maint_FGM1","abate_yr4_AUD_maint_FGM1","abate_yr5_AUD_maint_FGM1","abate_yr6_AUD_FGM1","abate_yr7_AUD_FGM1","abate_yr8_AUD_FGM1","abate_yr9_AUD_FGM1","abate_yr10_AUD_FGM1",     
                                                                  "abate_yr11_AUD_FGM1","abate_yr12_AUD_FGM1","abate_yr13_AUD_FGM1","abate_yr14_AUD_FGM1","abate_yr15_AUD_FGM1","abate_yr16_AUD_FGM1","abate_yr17_AUD_FGM1","abate_yr18_AUD_FGM1",     
                                                                  "abate_yr19_AUD_FGM1","abate_yr20_AUD_FGM1","abate_yr21_AUD_FGM1","abate_yr22_AUD_FGM1","abate_yr23_AUD_FGM1","abate_yr24_AUD_FGM1","abate_yr25_AUD_FGM1") %>% as.data.frame
CBA.CPhigh.maint.FGM2.df <- CBA.CPhigh.maint.FGM2 %>% dplyr::select("abate_yr1_AUD_maint_FGM2","abate_yr2_AUD_maint_FGM2",
                                                                    "abate_yr3_AUD_maint_FGM2","abate_yr4_AUD_maint_FGM2","abate_yr5_AUD_maint_FGM2","abate_yr6_AUD_FGM2","abate_yr7_AUD_FGM2","abate_yr8_AUD_FGM2","abate_yr9_AUD_FGM2","abate_yr10_AUD_FGM2",     
                                                                    "abate_yr11_AUD_FGM2","abate_yr12_AUD_FGM2","abate_yr13_AUD_FGM2","abate_yr14_AUD_FGM2","abate_yr15_AUD_FGM2","abate_yr16_AUD_FGM2","abate_yr17_AUD_FGM2","abate_yr18_AUD_FGM2",     
                                                                    "abate_yr19_AUD_FGM2","abate_yr20_AUD_FGM2","abate_yr21_AUD_FGM2","abate_yr22_AUD_FGM2","abate_yr23_AUD_FGM2","abate_yr24_AUD_FGM2","abate_yr25_AUD_FGM2") %>% as.data.frame
#check
colnames(CBA.CPlow.maint.FGM1.df)
colnames(CBA.CPlow.maint.FGM2.df)
colnames(CBA.CPhigh.maint.FGM1.df)
colnames(CBA.CPhigh.maint.FGM2.df)
#there are four scenarios needed - 2 with low carbon price and FGM1 and 2, 2 with high carbon price and FGM1 and FGM2

##upper
# Annuity GM with average CP across 25 years using upper CI for carbon abatement
CBA.CPlow.upper <- CBA.upper %>% mutate(across(starts_with("abate_yr"), funCP, .names = "{.col}_AUD"))

#mutate to minus off maintenance cost for first five years
CBA.CPlow.maint.upper <- CBA.CPlow.upper %>% mutate(across(ends_with("AUD"), funMaint, .names = "{.col}_maint"))

# mutate to minus off FGM #have two farm practices Class 1 and Class 2 - Class 2 is base (lower)
#minus FGM2 (default)
funFGM2a.upper <- function(x) (x-CBA.CPlow.maint.upper$FGM2)
CBA.CPlow.maint.FGM2.upper <- CBA.CPlow.maint.upper %>% mutate(across(contains("AUD"), funFGM2a.upper, .names = "{.col}_FGM2"))
colnames(CBA.CPlow.maint.FGM2.upper)
head(CBA.CPlow.maint.FGM2.upper$abate_yr1_AUD)
head(CBA.CPlow.maint.FGM2.upper$abate_yr1_AUD_FGM2)
#has calculated abatement AUD - FGM1 for each year with low carbon price 

# select columns needed
CBA.CPlow.maint.FGM2.df.upper <- CBA.CPlow.maint.FGM2.upper %>% dplyr::select("abate_yr1_AUD_maint_FGM2","abate_yr2_AUD_maint_FGM2", "abate_yr3_AUD_maint_FGM2","abate_yr4_AUD_maint_FGM2","abate_yr5_AUD_maint_FGM2",
                                                                            "abate_yr6_AUD_FGM2","abate_yr7_AUD_FGM2","abate_yr8_AUD_FGM2","abate_yr9_AUD_FGM2","abate_yr10_AUD_FGM2",     
                                                                            "abate_yr11_AUD_FGM2","abate_yr12_AUD_FGM2","abate_yr13_AUD_FGM2","abate_yr14_AUD_FGM2","abate_yr15_AUD_FGM2","abate_yr16_AUD_FGM2","abate_yr17_AUD_FGM2","abate_yr18_AUD_FGM2",     
                                                                            "abate_yr19_AUD_FGM2","abate_yr20_AUD_FGM2","abate_yr21_AUD_FGM2","abate_yr22_AUD_FGM2","abate_yr23_AUD_FGM2","abate_yr24_AUD_FGM2","abate_yr25_AUD_FGM2") %>% as.data.frame

##lower
# Annuity GM with average CP across 25 years using lower CI for carbon abatement
CBA.CPlow.lower <- CBA.lower %>% mutate(across(starts_with("abate_yr"), funCP, .names = "{.col}_AUD"))

#mutate to minus off maintenance cost for first five years
CBA.CPlow.maint.lower <- CBA.CPlow.lower %>% mutate(across(ends_with("AUD"), funMaint, .names = "{.col}_maint"))

# mutate to minus off FGM #have two farm practices Class 1 and Class 2 - Class 2 is base (lower)
#minus FGM1 (default)
funFGM2a.lower <- function(x) (x-CBA.CPlow.maint.lower$FGM2)
CBA.CPlow.maint.FGM2.lower <- CBA.CPlow.maint.lower %>% mutate(across(contains("AUD"), funFGM2a.lower, .names = "{.col}_FGM2"))
colnames(CBA.CPlow.maint.FGM2.lower)
head(CBA.CPlow.maint.FGM2.lower$abate_yr1_AUD)
head(CBA.CPlow.maint.FGM2.lower$abate_yr1_AUD_FGM2)
#has calculated abatement AUD - FGM2 for each year with low carbon price 

# select columns needed
CBA.CPlow.maint.FGM2.df.lower <- CBA.CPlow.maint.FGM2.lower %>% dplyr::select("abate_yr1_AUD_maint_FGM2","abate_yr2_AUD_maint_FGM2", "abate_yr3_AUD_maint_FGM2","abate_yr4_AUD_maint_FGM2","abate_yr5_AUD_maint_FGM2",
                                                                              "abate_yr6_AUD_FGM2","abate_yr7_AUD_FGM2","abate_yr8_AUD_FGM2","abate_yr9_AUD_FGM2","abate_yr10_AUD_FGM2",     
                                                                              "abate_yr11_AUD_FGM2","abate_yr12_AUD_FGM2","abate_yr13_AUD_FGM2","abate_yr14_AUD_FGM2","abate_yr15_AUD_FGM2","abate_yr16_AUD_FGM2","abate_yr17_AUD_FGM2","abate_yr18_AUD_FGM2",     
                                                                              "abate_yr19_AUD_FGM2","abate_yr20_AUD_FGM2","abate_yr21_AUD_FGM2","abate_yr22_AUD_FGM2","abate_yr23_AUD_FGM2","abate_yr24_AUD_FGM2","abate_yr25_AUD_FGM2") %>% as.data.frame


# RESTORATION COST PER HA AND WITH ECONOMIES OF SCALE

# FUNCTIONS
# NPV function to calculate NPV and then use mapply which takes vectors of lists (data frames)
# Payment 0 happens at time 1 (not time 0) 
npv1 <- function(i, cf, nn=length(cf)) {
  sum(cf * (1/(1+i))^(1:nn)) 
}

# Real net present value (NPV) accounting for DCF and restoration cost
NPV_C <- function(DCF, c) {
  DCF-c
}

# LOGGED SCENARIO
# NPV (discounted cash flow analysis for for 5 scenarios)

# NPV for 25 years, 1% discount rate, current carbon price, lower rest cost, and FGM2 (base)
CBA$DCF_25_4_CPlow_FGM2 <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM2.df)))  
head(CBA$DCF_25_4_CPlow_FGM2) # check function
# lower restoration cost
CBA$NPV_25_4_CPlow_FGM2_RClow <- NPV_C(CBA$DCF_25_4_CPlow_FGM2, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, current carbon price, upper rest cost, and FGM2
# upper restoration cost
CBA$NPV_25_4_CPlow_FGM2_RChigh <- NPV_C(CBA$DCF_25_4_CPlow_FGM2, c=CBA$rest_high_cost_scale)

# NPV  with 25 years, 4% discount rate, current carbon price, lower rest cost, and FGM2
CBA$DCF_25_7_CPlow_FGM2 <- mapply(npv1, i=0.04, cf=as.data.frame(t(CBA.CPlow.maint.FGM2.df)))
# lower restoration cost
CBA$NPV_25_7_CPlow_FGM2_RClow <- NPV_C(CBA$DCF_25_7_CPlow_FGM2, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, higher carbon price, lower rest cost, and FGM2
CBA$DCF_25_4_CPhigh_FGM2 <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPhigh.maint.FGM2.df))) 
# lower restoration cost
CBA$NPV_25_4_CPhigh_FGM2_RClow <- NPV_C(CBA$DCF_25_4_CPhigh_FGM2, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, current carbon price, lower rest cost and higher farm gross margin (FGM1)
CBA$DCF_25_4_CPlow_FGM1 <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM1.df))) 
# lower restoration cost
CBA$NPV_25_4_CPlow_FGM1_RClow <- NPV_C(CBA$DCF_25_4_CPlow_FGM1, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, current carbon price, lower rest cost, lower farm gross margin (FGM2), and higher carbon abatement
CBA$DCF_25_4_CPlow_FGM2_upper <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM2.df.upper))) 
# lower restoration cost
CBA$NPV_25_4_CPlow_FGM2_RClow_upper <- NPV_C(CBA$DCF_25_4_CPlow_FGM2_upper, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, current carbon price, lower rest cost, lower farm gross margin (FGM2), and lower carbon abatement
CBA$DCF_25_4_CPlow_FGM2_lower <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM2.df.lower))) 
# lower restoration cost
CBA$NPV_25_4_CPlow_FGM2_RClow_lower <- NPV_C(CBA$DCF_25_4_CPlow_FGM2_lower, c=CBA$rest_low_cost_scale)

# NPV for 100 years with base equation (1%, current carbon price, lower rest cost, FGM2)
### can't credit beyond 25 years, so don't include this scenario ###
# get into dataframe
#colnames(CBA.CPlow.maint.FGM2)
#CBA.CPlow.maint.FGM2.100.df <- CBA.CPlow.maint.FGM2 %>% dplyr::select(412:416,317:411) %>% as.data.frame
#colnames(CBA.CPlow.maint.FGM2.100.df) #check correct columns and that maintenance has been deducted for first five years

# NPV for 100 years with base equation 
#CBA$DCF_100_4_CPlow_FGM2 <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM2.100.df))) 
# lower restoration cost
#CBA$NPV_100_4_CPlow_FGM2_RClow <- NPV_C(CBA$DCF_100_4_CPlow_FGM2, c=CBA$rest_low_cost_scale)

# save file
write.csv(CBA, file="CBA.csv", row.names=F)
which(CBA$restor_ha>500) #no large sites


### Explore NPV ###
# Calculate which options are profitable
colnames(CBA)

#calculate number of profitable sites
NPV <- CBA %>% 
  dplyr::select("FID_site", "restor_ha","NPV_25_4_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM2_RChigh",  
         "NPV_25_4_CPhigh_FGM2_RClow", "NPV_25_7_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM1_RClow",
         "NPV_25_4_CPlow_FGM2_RClow_upper", "NPV_25_4_CPlow_FGM2_RClow_lower") %>% as.data.frame
head(NPV)

fun <- function(x) {
  length(which(x>0))
}

NPV.profit <- apply(NPV,2,fun) %>% as.data.frame
colnames(NPV.profit) <- "profit"
NPV.profit
NPV.profit$prop <- NPV.profit$profit/length(NPV$FID_site)
NPV.profit <- NPV.profit[-c(1:2),]
write.csv(NPV.profit, file="NPV_profitable.csv", row.names=T)

# calculate area of profitable sites across scenarios
NPV.base <- subset(NPV, NPV$NPV_25_4_CPlow_FGM2_RClow>0)
area.profit.base <- sum(NPV.base$restor_ha)
area.profit.base/sum(NPV$restor_ha) 

NPV.7 <- subset(NPV, NPV$NPV_25_7_CPlow_FGM2_RClow>0)
area.profit.7 <- sum(NPV.7$restor_ha)
area.profit.7/sum(NPV$restor_ha) 

NPV.CPhigh <- subset(NPV, NPV$NPV_25_4_CPhigh_FGM2_RClow>0)
area.profit.CPhigh <- sum(NPV.CPhigh$restor_ha)
area.profit.CPhigh/sum(NPV$restor_ha) 

NPV.RChigh <- subset(NPV, NPV$NPV_25_4_CPlow_FGM2_RChigh>0)
area.profit.RChigh <- sum(NPV.RChigh$restor_ha)
area.profit.RChigh/sum(NPV$restor_ha) 

NPV.FGM1 <- subset(NPV, NPV$NPV_25_4_CPlow_FGM1_RClow>0)
area.profit.FGM1 <- sum(NPV.FGM1$restor_ha)
area.profit.FGM1/sum(NPV$restor_ha) 

NPV.upper <- subset(NPV, NPV$NPV_25_4_CPlow_FGM2_RClow_upper>0)
area.profit.upper <- sum(NPV.upper$restor_ha)
area.profit.upper/sum(NPV$restor_ha) 

NPV.lower <- subset(NPV, NPV$NPV_25_4_CPlow_FGM2_RClow_lower>0)
area.profit.lower <- sum(NPV.lower$restor_ha)
area.profit.lower/sum(NPV$restor_ha) 

#NPV.100 <- subset(NPV, NPV$NPV_100_4_CPlow_FGM2_RClow>0)
#area.profit.100 <- sum(NPV.100$restor_ha)
#area.profit.100/sum(NPV$restor_ha) 

write.csv(NPV.profit, "NPV.profit.csv", row.names= T)

#what is going on???
CBA.CPlow.maint.FGM2[1:43,c(111:135)]
# years without maintenance costs, i.e. from year 5-10 make a profit (depending on carbon price), but with the high one-off restoration cost, not enough to offset this over the time period


#make barplot
greys7 <- gray.colors(7, start = 0, end = 0.8, gamma = 2.2, alpha = NULL) #greyscale for manuscript (5 shades)
NPV.profit
NPV_names<-c("S1:25yr 1% CP57 lowRC FGMa", "S2:25yr 1% CP57 highRC FGMa ", "S3:25yr 1% CP132 lowRC FGMa", "S4:25yr 4% CP57 lowRC FGMa", "S5:25yr 1% CP57 lowRC FGMb", "S6:25yr 1% CP57 lowRC FGMa upperCI", "S7:25yr 1% CP57 lowRC FGMa lowerCI")
tiff("NPV_barplot.tif", units="mm", width=95, height=80, res=800, compression = "lzw")
par(mfrow=c(1,1))
par(mar=c(5,5,1,1), mgp=c(4,0.5,0), tck=-0.01)
barplot(NPV.profit$prop, beside = T, xlab="", cex.names = 0.5, ylim=c(0,1), axis.lty=1,
        ylab= "Proportion of profitable sites", las = 2, cex.lab = 1.0, col="grey40", names.arg = NPV_names, border = NA)
dev.off()


# Determine rankings of NPV
CBA$NPV_25_4_CPlow_FGM2_RClow_rank <- rank(-CBA$NPV_25_4_CPlow_FGM2_RClow)
CBA$NPV_25_4_CPhigh_FGM2_RClow_rank <- rank(-CBA$NPV_25_4_CPhigh_FGM2_RClow)
CBA$NPV_25_4_CPlow_FGM2_RChigh_rank <- rank(-CBA$NPV_25_4_CPlow_FGM2_RChigh)
CBA$NPV_25_4_CPlow_FGM1_RClow_rank <- rank(-CBA$NPV_25_4_CPlow_FGM1_RClow)
CBA$NPV_25_7_CPlow_FGM2_RClow_rank <- rank(-CBA$NPV_25_7_CPlow_FGM2_RClow)
CBA$NPV_25_4_CPlow_FGM2_RClow_upper_rank <- rank(-CBA$NPV_25_4_CPlow_FGM2_RClow_upper)
CBA$NPV_25_4_CPlow_FGM2_RClow_lower_rank <- rank(-CBA$NPV_25_4_CPlow_FGM2_RClow_lower)
#CBA$NPV_100_4_CPlow_FGM2_RClow_rank <- rank(-CBA$NPV_100_4_CPlow_FGM2_RClow)

#get best ranking sites
NPV.rank <- CBA %>% 
  dplyr::select(NPV_25_4_CPlow_FGM2_RClow_rank, NPV_25_4_CPhigh_FGM2_RClow_rank, NPV_25_4_CPlow_FGM2_RChigh_rank, NPV_25_7_CPlow_FGM2_RClow_rank, NPV_25_4_CPlow_FGM1_RClow_rank, NPV_25_4_CPlow_FGM2_RClow_upper, NPV_25_4_CPlow_FGM2_RClow_lower) #NPV_100_4_CPlow_FGM2_RClow_rank, 
colnames(NPV.rank)
which.top.10 <- function(x) {
  which(x<=10)
}
NPV.top10 <- apply(NPV.rank, 2, which.top.10)
NPV.top10


#add in basins
rsites.basins <- read_excel("rsites_PeelSW_basins.xls", sheet="rsites_PeelSW_basins") #update with file name
head(rsites.basins)
rsites.basins <- rsites.basins %>% dplyr::select(FID_site="FID_PeelHa", "BASIN_NAME")
#add to NPV dataframe
NPV <- full_join(NPV, rsites.basins, by="FID_site")
head(NPV)

# Figure - boxplot of NPV and barplot of NPV profit areas per basin - add barplot of NPV profit sites per landuse
NPV.plot <- NPV[,-c(1:2,10)]
head(NPV.plot) # can't log as some are negative
range <- apply(NPV.plot, 2, range)
med <- apply(NPV.plot, 2, median)
mean <- apply(NPV.plot, 2, mean)
range(NPV$NPV_25_4_CPhigh_FGM2_RClow)

#figure of prop of profitable sites per basin
NPV.basin <- NPV %>% 
  dplyr::select("FID_site", "restor_ha","BASIN_NAME", "NPV_25_4_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM2_RChigh",  
         "NPV_25_4_CPhigh_FGM2_RClow", "NPV_25_7_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM1_RClow", "NPV_25_4_CPlow_FGM2_RClow_upper","NPV_25_4_CPlow_FGM2_RClow_lower" ) %>% as.data.frame #"NPV_100_4_CPlow_FGM2_RClow", 
head(NPV.basin)

NPV.profit.basin.S1 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_FGM2_RClow>0)
NPV.profit.basin.S1.area <- with(NPV.profit.basin.S1, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S1 <- NPV.profit.basin.S1 %>% 
   dplyr::select("NPV_25_4_CPlow_FGM2_RClow", "BASIN_NAME") 
NPV.profit.basin.S1 <- NPV.profit.basin.S1[,-1]
NPV.profit.basin.S1.freq <- table(NPV.profit.basin.S1) %>% as.matrix
prop.table(NPV.profit.basin.S1.freq,2)

NPV.profit.basin.S2 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_FGM2_RChigh>0)
NPV.profit.basin.S2.area <- with(NPV.profit.basin.S2, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S2 <- NPV.profit.basin.S2 %>% 
dplyr::select("NPV_25_4_CPlow_FGM2_RChigh", "BASIN_NAME") 
NPV.profit.basin.S2 <- NPV.profit.basin.S2[,-1]
NPV.profit.basin.S2.freq <- table(NPV.profit.basin.S2) %>% as.matrix
prop.table(NPV.profit.basin.S2.freq,2) #none
 
NPV.profit.basin.S3 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPhigh_FGM2_RClow>0)
NPV.profit.basin.S3.area <- with(NPV.profit.basin.S3, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S3 <- NPV.profit.basin.S3 %>% 
dplyr::select("NPV_25_4_CPhigh_FGM2_RClow", "BASIN_NAME") 
NPV.profit.basin.S3 <- NPV.profit.basin.S3[,-1]
NPV.profit.basin.S3.freq <- table(NPV.profit.basin.S3) %>% as.matrix
prop.table(NPV.profit.basin.S3.freq,2) 
 
NPV.profit.basin.S4 <- subset(NPV.basin, NPV.basin$NPV_25_7_CPlow_FGM2_RClow>0)
NPV.profit.basin.S4.area <- with(NPV.profit.basin.S4, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S4 <- NPV.profit.basin.S4 %>% 
dplyr::select("NPV_25_7_CPlow_FGM2_RClow", "BASIN_NAME") 
NPV.profit.basin.S4 <- NPV.profit.basin.S4[,-1]
NPV.profit.basin.S4.freq <- table(NPV.profit.basin.S4) %>% as.matrix
prop.table(NPV.profit.basin.S4.freq,2) 

#don't include 100 year scenario, as max crediting is 25 years for ACCUs 
#NPV.profit.basin.S5 <- subset(NPV.basin, NPV.basin$NPV_100_4_CPlow_FGM2_RClow>0)
#NPV.profit.basin.S5.area <- with(NPV.profit.basin.S5, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
#NPV.profit.basin.S5 <- NPV.profit.basin.S5 %>% 
#dplyr::select("NPV_100_4_CPlow_FGM2_RClow", "BASIN_NAME") 
#NPV.profit.basin.S5 <- NPV.profit.basin.S5[,-1]
#NPV.profit.basin.S5.freq <- table(NPV.profit.basin.S5) %>% as.matrix
#prop.table(NPV.profit.basin.S5.freq,2)

NPV.profit.basin.S5 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_FGM1_RClow>0)
NPV.profit.basin.S5.area <- with(NPV.profit.basin.S5, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S5 <- NPV.profit.basin.S5 %>% 
  dplyr::select("NPV_25_4_CPlow_FGM1_RClow", "BASIN_NAME") 
NPV.profit.basin.S5 <- NPV.profit.basin.S5[,-1]
NPV.profit.basin.S5.freq <- table(NPV.profit.basin.S5) %>% as.matrix
prop.table(NPV.profit.basin.S5.freq,2)

NPV.profit.basin.S6 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_FGM2_RClow_upper>0)
NPV.profit.basin.S6.area <- with(NPV.profit.basin.S6, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S6 <- NPV.profit.basin.S6 %>% 
  dplyr::select("NPV_25_4_CPlow_FGM2_RClow_upper", "BASIN_NAME") 
NPV.profit.basin.S6 <- NPV.profit.basin.S6[,-1]
NPV.profit.basin.S6.freq <- table(NPV.profit.basin.S6) %>% as.matrix
prop.table(NPV.profit.basin.S6.freq,2)

NPV.profit.basin.S7 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_FGM2_RClow_lower>0)
NPV.profit.basin.S7.area <- with(NPV.profit.basin.S7, tapply(restor_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S7 <- NPV.profit.basin.S7 %>% 
  dplyr::select("NPV_25_4_CPlow_FGM2_RClow_lower", "BASIN_NAME") 
NPV.profit.basin.S7 <- NPV.profit.basin.S7[,-1]
NPV.profit.basin.S7.freq <- table(NPV.profit.basin.S7) %>% as.matrix
prop.table(NPV.profit.basin.S7.freq,2)
 
#join dataframe
NPV.profit.basin.S1.freq <- NPV.profit.basin.S1.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S2.freq <- NPV.profit.basin.S2.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S3.freq <- NPV.profit.basin.S3.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S4.freq <- NPV.profit.basin.S4.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S5.freq <- NPV.profit.basin.S5.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S6.freq <- NPV.profit.basin.S6.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin") 
NPV.profit.basin.S7.freq <- NPV.profit.basin.S7.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin") 

NPV.profit.basin.all <- full_join(NPV.profit.basin.S1.freq, NPV.profit.basin.S2.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S3.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S4.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S5.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S6.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S7.freq, by="Basin")

NPV.profit.basin.all <- column_to_rownames(NPV.profit.basin.all, var="Basin")

colnames(NPV.profit.basin.all) <- c("NPV_25_4_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM2_RChigh",  
                                    "NPV_25_4_CPhigh_FGM2_RClow", "NPV_25_7_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM1_RClow", "NPV_25_4_CPlow_FGM2_RClow_upper", "NPV_25_4_CPlow_FGM2_RClow_lower") #"NPV_100_4_CPlow_FGM2_RClow", 
#change NAs to 0
NPV.profit.basin.all[is.na(NPV.profit.basin.all)] <- 0
NPV.profit.basin.all <- NPV.profit.basin.all %>% as.matrix
 
#combine area per basin
NPV.profit.basin.S1.area <- NPV.profit.basin.S1.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S2.area <- NPV.profit.basin.S2.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S3.area <- NPV.profit.basin.S3.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S4.area <- NPV.profit.basin.S4.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S5.area <- NPV.profit.basin.S5.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S6.area <- NPV.profit.basin.S6.area %>% tibble::rownames_to_column("Basin") 
NPV.profit.basin.S7.area <- NPV.profit.basin.S7.area %>% tibble::rownames_to_column("Basin") 

NPV.profit.basin.area <- full_join(NPV.profit.basin.S1.area, NPV.profit.basin.S2.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S3.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S4.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S5.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S6.area, by="Basin") 
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S7.area, by="Basin") 

NPV.profit.basin.area <- column_to_rownames(NPV.profit.basin.area, var="Basin")
colnames(NPV.profit.basin.area) <- c("NPV_25_4_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM2_RChigh",  
                                     "NPV_25_4_CPhigh_FGM2_RClow", "NPV_25_7_CPlow_FGM2_RClow", "NPV_25_4_CPlow_FGM1_RClow", "NPV_25_4_CPlow_FGM2_RClow_upper", "NPV_25_4_CPlow_FGM2_RClow_lower") #"NPV_100_4_CPlow_FGM2_RClow", 
 
#change NAs to 0
NPV.profit.basin.area[is.na(NPV.profit.basin.area)] <- 0
NPV.profit.basin.area <- NPV.profit.basin.area %>% as.matrix

# restorable area per basin
rest.area.basins <- with(NPV, tapply(restor_ha, BASIN_NAME, sum)) %>% as.data.frame
colnames(rest.area.basins) <- "rest_area_ha"
prop.table(rest.area.basins) 

#save dataframes
write.csv(NPV, "NPV.df.csv", row.names = F)
write.csv(CBA, "CBA.df.csv", row.names = F)
write.csv(NPV.profit.basin.area, "NPV_profit_basins_area.csv", row.names= T)


#multi-panel fig with restoration area, carbon abatement, NPV scenarios and profitable sites across basins
#calculate carbon abate
CBA <- full_join(CBA, rsites.basins, by="FID_site")
CBA$abate.yr.ha <- (CBA$abate_CO2eMg_25yrs/25)/CBA$restor_ha
abate.yr.ha.basin.mean <- with(CBA, tapply(abate.yr.ha, BASIN_NAME, mean)) %>% as.data.frame
abate.yr.ha.basin.n <- with(CBA, tapply(abate.yr.ha, BASIN_NAME, length)) %>% as.data.frame
abate.yr.ha.basin.var <- with(CBA, tapply(abate.yr.ha, BASIN_NAME, var)) %>% as.data.frame
abate.yr.ha.basin.se <- sqrt(abate.yr.ha.basin.var/abate.yr.ha.basin.n) %>% as.data.frame
abate.yr.ha.basin.upper <- abate.yr.ha.basin.mean + abate.yr.ha.basin.se
abate.yr.ha.basin.lower <- abate.yr.ha.basin.mean - abate.yr.ha.basin.se

abate.yr.ha.basin.mean.plot <- abate.yr.ha.basin.mean %>% as.matrix %>% as.vector
abate.yr.ha.basin.upper.plot <- abate.yr.ha.basin.upper %>% as.matrix %>% as.vector
abate.yr.ha.basin.lower.plot <- abate.yr.ha.basin.lower %>% as.matrix %>% as.vector

#mean
abate.yr.ha.mean <- mean(CBA$abate.yr.ha)
abate.yr.ha.range <- range(CBA$abate.yr.ha)
abate.yr.ha.n <- length(CBA$abate.yr.ha)
abate.yr.ha.var <- var(CBA$abate.yr.ha)
abate.yr.ha.se <- sqrt(abate.yr.ha.var/abate.yr.ha.n) 

##upper
CBA.upper$abate_CO2eMg_yr_ha <- (CBA.upper$abate_CO2eMg_25yrs/25)/CBA.upper$restor_ha
abate.yr.ha.mean.upper <- mean(CBA.upper$abate_CO2eMg_yr_ha)
abate.yr.ha.length.upper <- length(CBA.upper$abate_CO2eMg_yr_ha) 
abate.yr.ha.var.upper <- var(CBA.upper$abate_CO2eMg_yr_ha)
abate.yr.ha.se.upper <- sqrt(abate.yr.ha.var.upper/abate.yr.ha.length.upper)

##lower
CBA.lower$abate_CO2eMg_yr_ha <- (CBA.lower$abate_CO2eMg_25yrs/25)/CBA.lower$restor_ha
abate.yr.ha.mean.lower <- mean(CBA.lower$abate_CO2eMg_yr_ha)
abate.yr.ha.length.lower <- length(CBA.lower$abate_CO2eMg_yr_ha) 
abate.yr.ha.var.lower <- var(CBA.lower$abate_CO2eMg_yr_ha)
abate.yr.ha.se.lower <- sqrt(abate.yr.ha.var.lower/abate.yr.ha.length.lower)


tiff("fig.rest.carbon.NPV.tif", units="mm", width=190, height=190, res=800, compression = "lzw")
par(mfrow=c(2,2), mar=c(7.2,5,1,1), mgp=c(4,0.3,0), tck=-0.01)
NPV_names
basin_names <- rownames(rest.area.basins) 
library(RColorBrewer)
basin.palette <- brewer.pal(7, "Paired")
#display.brewer.pal(7, "Paired")

# #1 potential restorable area
barplot(rest.area.basins$rest_area_ha, names.arg=rownames(rest.area.basins), cex.names=0.8, cex.lab=1.0, ylab="Potential restoration area", las=2, ylim=c(0,400))
mtext("a", line=0, side=3, adj=0, cex=1.3)

# #2 carbon abatement variation among basins
plot(abate.yr.ha.basin.mean.plot ~ c(1:length(abate.yr.ha.basin.mean.plot)), xlab="", ylab="Mean carbon abatement (Mg CO2-e ha-1 yr-1)", 
    pch=19, xaxt="n", las=2, ylim=c(5, 15)) 
arrows(c(1:length(abate.yr.ha.basin.mean.plot)), abate.yr.ha.basin.upper.plot, c(1:length(abate.yr.ha.basin.mean.plot)), abate.yr.ha.basin.lower.plot, length=0.05, angle=90, code=3) ## add standard errors
axis(side=1, at=c(1:length(abate.yr.ha.basin.mean.plot)), labels=rownames(abate.yr.ha.basin.mean), cex.axis=0.8, las=2)  ## add category names to x axis
mtext("b", line=0, side=3, adj=0, cex=1.3)

#3 NPV among scenarios
boxplot(NPV.plot, xlab="", ylab="Net present value (AUD)", las = 2, names = NPV_names, cex.axis=0.6, cex.lab=1.0)
text(1, 1000000, "2298 ", cex=0.8)
text(2, 1000000, "-178434", cex=0.8)
text(3, 1000000, "54402", cex=0.8)
text(4, 1000000, "-10709", cex=0.8)
text(5, 1000000, "-29711", cex=0.8)
text(6, 1000000, "8587", cex=0.8)
text(7, 1000000, "-2964", cex=0.8)
mtext("c", line=0, side=3, adj=0, cex=1.3)
dev.off()

#4 area of profitable sites across basins
# none profitable, so don't include
# barplot(NPV.profit.basin.area, col=basin.palette, las=2, names.arg=NPV_names, cex.names=0.75, cex.lab=1.0, ylab="Area of profitable sites (ha)", ylim=c(0,14000))
# legend(x=1, y=8000, legend=basin_names, fill=basin.palette, border=NA, cex=0.8)
# mtext("d", line=0, side=3, adj=0, cex=1.5)



# Compare NPV scenarios with dendograms and ordination
#do this when NPV has been finalised
NPV.matrix <- NPV.plot %>% as.matrix
head(NPV.matrix)
NPV.matrix[,2] #NAs gone
# create dissimilarity matrix - all columns add to 1
library(vegan)
NPV.d.matrix <- prop.table(NPV.matrix, margin=2) #not working, giving NAs
NPV.d.matrix.t <- t(NPV.d.matrix)

# dissimiliarity matrix with bray curtis
dist.NPV.jac.t <- vegdist(NPV.d.matrix.t, method="bray")

#construct the dendogram by scenario
hc.NPV.jac.t <- hclust(dist.NPV.jac.t, method="complete")  

#Construct the nMDS ordination by scenarios
NPV.t.mds <- metaMDS(dist.NPV.jac.t,2)

#---plot for paper---
tiff("fig.NPV.dendo.ord.tif", units="mm", width=190, height=95, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(hc.NPV.jac.t, labels=NPV_names, xlab = "Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("a", line=0, side=3, adj=0, cex=1.3)
plot(NPV.t.mds$points, type='n', xlab='NMDS1', ylab='NMDS2')
ordilabel(NPV.t.mds$points, lab=NPV_names, cex=0.5)
mtext("b", line=0, side=3, adj=0, cex=1.3)
dev.off()

tiff("fig.NPV.dendo.tif", units="mm", width=95, height=95, res=800, compression = "lzw")
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(hc.NPV.jac.t, labels=NPV_names, xlab = "NPV Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()
