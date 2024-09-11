# Economic analysis
# Valerie Hagger 13/02/2024
rm(list = ls())
setwd("R:/NESP115-Q4418/R_working_directory/Fitzroy") #sets the working directory

# load packages
library(tidyverse)
library(readxl)


# ---S2--- FARM GROSS MARGINS

# Add GM for cattle from Star et al. 2017 - AUD/AE
#1	312.55	heavier Japanese-oxen
#2	241.98	Not specified
#3	199.3	Not specified
#4	197.43	Not specified
#5	182.32	light store cattle
gm_prod_groups <- read.csv("GM_prod_groups_Star2017.csv", header=T)

# escalate to Dec 2022 from June 2015 using CPI (Index groups, All groups, Brisbane)
CPI_Jun15_Dec22 <- (132.1-107.4)/107.4
gm_prod_groups$GM_AE_21<-gm_prod_groups$gross_margin_AE*(1+CPI_Jun15_Dec22)

# add stocking densities for each site
glm_prod_groups <- read.csv("GLM_prod_groups.csv", header=T)
glm_land_types <- read.csv("GLM_land_types.csv", header=T)

#glm_land_types$prod_group <- glm_prod_groups$Productivity_grouping[match(glm_land_types$land_type, glm_prod_groups$Land_type)] #adds country codes to dataset
head(glm_land_types)
table(glm_land_types$land_type)
table(glm_land_types$LTYPE1_NAM)
table(glm_prod_groups$Land_type)

# identify GLMs for each site
# Area of GLMs per site with hydrological modified wetlands incorporated
rsites.GLM <- read_excel("rsites4_GLM_hydromod.xls", sheet="rsites4_GLM_hydromod")
head(rsites.GLM)
#add freshwater plains to blanks
rsites.GLM$GLM <- ifelse(is.na(rsites.GLM$LTYPE1_NAM), rsites.GLM$HYDROMOD, rsites.GLM$LTYPE1_NAM)
table(rsites.GLM$GLM)

#match GLM types to new land types
rsites.GLM$land_type <- glm_land_types$land_type[match(rsites.GLM$GLM, glm_land_types$LTYPE1_NAM)] #adds assigned land types to GLMs in dataset
table(rsites.GLM$land_type)

rsites.GLM$prod_group <- glm_prod_groups$Productivity_grouping[match(rsites.GLM$land_type, glm_prod_groups$Land_type)] #adds productivity groupings
rsites.GLM$AEha <- glm_prod_groups$AE_ha[match(rsites.GLM$land_type, glm_prod_groups$Land_type)] #adds productivity groupings
rsites.GLM$GM_AE <- gm_prod_groups$GM_AE_21[match(rsites.GLM$prod_group, gm_prod_groups$prod_group)] #adds productivity groupings

#calculate farm gross margin per year per land type of each site
rsites.GLM$FGM <- with(rsites.GLM, GLM_ha*AEha*GM_AE)
head(rsites.GLM) #check calc

#sum farm gross margin per site
# select and spread data from long to wide
colnames(rsites.GLM)
rsites.GLM.tidy <- rsites.GLM %>% 
  dplyr::select(FID_site="FID_tidal_", "Area_ha", "GLM", "FGM") %>% 
  spread(key = "GLM", value = "FGM", convert=TRUE) 
colnames(rsites.GLM.tidy)
rsites.GLM.tidy$FGM_yr <- rowSums(rsites.GLM.tidy[,3:28], na.rm=T)
rsites.GLM.tidy$Area_ha
rsites.GLM.tidy[352,] %>% as.matrix
#some very large sites 4000 ha have very high FGMs
rsites.GLM.tidy <- rsites.GLM.tidy %>% dplyr::select(FID_site, Area_ha, FGM_yr)

# assign land uses based on productivity and to account for different income
rsites.LU <- read_excel("rsites4_LU.xls", sheet="rsites4_LU")
table(rsites.LU$ALUM_code_)

Defence <- "1.3.1" #none
graz <- c("2.1.0", "3.2.2", "4.2.0", "4.2.1", "6.5.2", "6.2.2") 
hydromod <- c("1.3.0", "1.3.3", "1.1.3", "1.1.4", "1.1.7", "5.3.0", "5.3.3", "5.4.2", "5.4.3", "5.4.5", "5.5.3", "5.8.3", 
              "6.1.0", "6.2.1", "6.2.2", "6.2.3", "6.3.0", "6.5.0", "6.5.4") #or take hydromod wetlands


# ---S4--- RESTORATION COSTS
maint_AUD_5 <- 750
wetland_lower <- 5000
wetland_upper <- 275130
maint_AUD_25 <- 0

# Read in and manipulate mangrove cost data from Bayraktarov et al 2016
Bayrak.mangrove <- read.csv("Bayraktarov_2016_mangroves_section.csv", header=T)
colnames(Bayrak.mangrove)
library(tidyverse)
Bayrak.mangrove.costs <- Bayrak.mangrove %>% 
  dplyr::select("Specific.technique", "Country", economy.type="Economy.type..developed.or.developing.", restoration.cost="Restoration.cost.in...US....ha...2010.base..CPI")
head(Bayrak.mangrove.costs)
xc.AUD.USD.2010<-1.09016
Bayrak.mangrove.costs$restoration.cost.AUD<-Bayrak.mangrove.costs$restoration.cost*xc.AUD.USD.2010 #convert costs to AUD in 2010 exchange rate from Penn Table

# escalate to Dec 2022 from Dec 2010 using CPI (Index groups, All groups, Brisbane)
CPI_Dec10_Dec22 <- (132.1-97.4)/97.4
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

### "habitat conversion" applys to restoration of disused shrimp and aquaculture ponds, "Hydrological restoration - facilitation of a natural recovery" applys to this project

# mean and median of mangrove restoration costs in Australia - note only costs for planting exist
mangrove.costs.Aus<-subset(Bayrak.mangrove.costs, Bayrak.mangrove.costs$Country == "Australia") #subset of Australia only data
mangrove.costs.Aus.tech.med<-with(mangrove.costs.Aus, tapply(restoration.cost.AUD, Specific.technique, median, na.rm=T)) %>% as.matrix
median(mangrove.costs.Aus$restoration.cost.AUD, na.rm=T)
mean(mangrove.costs.Aus$restoration.cost.AUD, na.rm=T)

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


# ---S5--- COST BENEFIT ANALYSIS 
# Read in data with carbon benefit from R code 1 using median values and upper and lower 95% for selected parameters (AGB, BBG, and soil carbon)
CBA <- read.csv("rest.sites.abatement.pa.25yrs.csv", header=T)
CBA <- CBA[,-1]
CBA.upper <- read.csv("rest.sites.abatement.upperCI.pa.25yrs.csv", header=T)
CBA.upper <- CBA.upper[,-1]
CBA.lower <- read.csv("rest.sites.abatement.lowerCI.pa.25yrs.csv", header=T)
CBA.lower <- CBA.lower[,-1]
#add areas
rsites.basins <- read_excel("rsites4_basins.xls", sheet="rsites4_basins")
colnames(rsites.basins)
rsites.basins <- rsites.basins %>% dplyr::select("FID_site"=FID_tidal_, Area_ha, BASIN_NAME)
CBA <- full_join(rsites.basins, CBA, by="FID_site")
CBA.upper <- full_join(rsites.basins, CBA.upper, by="FID_site")
CBA.lower <- full_join(rsites.basins, CBA.lower, by="FID_site")
#add FGMS
colnames(rsites.GLM.tidy) 
rsites.FGM <- rsites.GLM.tidy %>% dplyr::select(FID_site, FGM_yr) #some sites don't have a FGM, therefore when joined there are NAs in FGMs
CBA <- full_join(CBA,rsites.FGM, by="FID_site")
colnames(CBA) #check
list(CBA$FGM_yr) 
#convert NAs to 0
CBA$FGM_yr[is.na(CBA$FGM_yr)] <- 0

CBA.upper <- full_join(CBA.upper,rsites.FGM, by="FID_site")
list(CBA.upper$FGM_yr) #check
CBA.upper$FGM_yr[is.na(CBA.upper$FGM_yr)] <- 0 #convert NAs to 0

CBA.lower <- full_join(CBA.lower,rsites.FGM, by="FID_site")
list(CBA.lower$FGM_yr) #check
CBA.lower$FGM_yr[is.na(CBA.lower$FGM_yr)] <- 0 #convert NAs to 0

#---INCORPORATE ECONOMIES OF SCALE---
range(CBA$Area_ha) 

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
CBA$rate_scale <- ifelse(CBA$Area_ha>=5,rate5,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$Area_ha>=10,rate10,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$Area_ha>=25,rate25,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$Area_ha>=50,rate50,CBA$rate_scale)
CBA$rate_scale <- ifelse(CBA$Area_ha>=100,rate100,CBA$rate_scale)
head(CBA$rate_scale)

#calculate scaled maintenance costs
CBA$maint_cost_scale <- (maint_AUD_5+(maint_AUD_5*CBA$rate_scale))*CBA$Area_ha
list(CBA$rate_scale) #check
list(CBA$Area_ha)
head(CBA$maint_cost_scale)

#calculate scaled restoration costs
CBA$rest_low_cost_scale <- (saltmarsh.cost.hydro.AUD.ha.med+(saltmarsh.cost.hydro.AUD.ha.med*CBA$rate_scale))*CBA$Area_ha
CBA$rest_high_cost_scale <- (mangrove.cost.hydro.AUD.ha.med+(mangrove.cost.hydro.AUD.ha.med*CBA$rate_scale))*CBA$Area_ha
head(CBA$rate_scale)
head(CBA$Area_ha)
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

#mutate to minus off maintance cost for first five years
funMaint <- function(x) (x-CBA.CPlow$maint_cost_scale)
CBA.CPlow.maint <- CBA.CPlow %>% mutate(across(ends_with("AUD"), funMaint, .names = "{.col}_maint"))
                                               #funs(.-CBA.CPlow$maint_cost_scale)
colnames(CBA.CPlow.maint) #check new vars
head(CBA.CPlow.maint$abate_yr1_AUD)
head(CBA.CPlow.maint$abate_yr1_AUD_maint)

funMaint2 <- function(x) (x-CBA.CPhigh$maint_cost_scale)
CBA.CPhigh.maint <- CBA.CPhigh %>% mutate(across(ends_with("AUD"), funMaint2, .names = "{.col}_maint"))
                                                #funs(.-CBA.CPhigh$maint_cost_scale) 
colnames(CBA.CPhigh.maint) #check new vars
head(CBA.CPhigh.maint$abate_yr1_AUD)
head(CBA.CPhigh.maint$abate_yr1_AUD_maint)

# mutate to minus off FGM (class B and class C) #only have one farm practice
funFGM <- function(x) (x-CBA.CPlow.maint$FGM_yr)
CBA.CPlow.maint.FGM <- CBA.CPlow.maint %>% mutate(across(contains("AUD"), funFGM, .names = "{.col}_FGM"))
  #funs(.-CBA.CPlow.maint$FGM_yr)
colnames(CBA.CPlow.maint.FGM)
head(CBA.CPlow.maint.FGM$abate_yr1_AUD)
head(CBA.CPlow.maint.FGM$abate_yr1_AUD_FGM)

funFGM2 <- function(x) (x-CBA.CPhigh.maint$FGM_yr)
CBA.CPhigh.maint.FGM <- CBA.CPhigh.maint %>% mutate(across(contains("AUD"),funFGM2, .names = "{.col}_FGM"))
  #funs(.-CBA.CPhigh.maint$FGM_yr)
colnames(CBA.CPhigh.maint.FGM)
head(CBA.CPhigh.maint.FGM$abate_yr1_AUD)
head(CBA.CPhigh.maint.FGM$abate_yr1_AUD_FGM)

# select columns needed
CBA.CPlow.maint.FGM.df <- CBA.CPlow.maint.FGM %>% dplyr::select("abate_yr1_AUD_maint_FGM","abate_yr2_AUD_maint_FGM",
   "abate_yr3_AUD_maint_FGM","abate_yr4_AUD_maint_FGM","abate_yr5_AUD_maint_FGM","abate_yr6_AUD_FGM","abate_yr7_AUD_FGM","abate_yr8_AUD_FGM","abate_yr9_AUD_FGM","abate_yr10_AUD_FGM",     
   "abate_yr11_AUD_FGM","abate_yr12_AUD_FGM","abate_yr13_AUD_FGM","abate_yr14_AUD_FGM","abate_yr15_AUD_FGM","abate_yr16_AUD_FGM","abate_yr17_AUD_FGM","abate_yr18_AUD_FGM",     
   "abate_yr19_AUD_FGM","abate_yr20_AUD_FGM","abate_yr21_AUD_FGM","abate_yr22_AUD_FGM","abate_yr23_AUD_FGM","abate_yr24_AUD_FGM","abate_yr25_AUD_FGM") %>% as.data.frame
CBA.CPhigh.maint.FGM.df <- CBA.CPhigh.maint.FGM %>% dplyr::select("abate_yr1_AUD_maint_FGM","abate_yr2_AUD_maint_FGM",
    "abate_yr3_AUD_maint_FGM","abate_yr4_AUD_maint_FGM","abate_yr5_AUD_maint_FGM","abate_yr6_AUD_FGM","abate_yr7_AUD_FGM","abate_yr8_AUD_FGM","abate_yr9_AUD_FGM","abate_yr10_AUD_FGM",     
    "abate_yr11_AUD_FGM","abate_yr12_AUD_FGM","abate_yr13_AUD_FGM","abate_yr14_AUD_FGM","abate_yr15_AUD_FGM","abate_yr16_AUD_FGM","abate_yr17_AUD_FGM","abate_yr18_AUD_FGM",     
    "abate_yr19_AUD_FGM","abate_yr20_AUD_FGM","abate_yr21_AUD_FGM","abate_yr22_AUD_FGM","abate_yr23_AUD_FGM","abate_yr24_AUD_FGM","abate_yr25_AUD_FGM") %>% as.data.frame

head(CBA.CPlow.maint.FGM.df) #check
head(CBA.CPhigh.maint.FGM.df)
colnames(CBA.CPlow.maint.FGM.df)
colnames(CBA.CPhigh.maint.FGM.df)


##upper
# Annuity GM with average CP across 25 years
CBA.CPlow.upper <- CBA.upper %>% mutate(across(starts_with("abate_yr"), funCP, .names = "{.col}_AUD"))
CBA.CPhigh.upper <- CBA.upper %>% mutate(across(starts_with("abate_yr"), funCPhigh, .names = "{.col}_AUD"))

#mutate to minus off maintenance cost for first five years
CBA.CPlow.maint.upper <- CBA.CPlow.upper %>% mutate(across(ends_with("AUD"), funMaint, .names = "{.col}_maint"))
CBA.CPhigh.maint.upper <- CBA.CPhigh.upper %>% mutate(across(ends_with("AUD"), funMaint2, .names = "{.col}_maint"))

# mutate to minus off FGM #only have one farm practice for Fitzroy
CBA.CPlow.maint.FGM.upper <- CBA.CPlow.maint.upper %>% mutate(across(contains("AUD"), funFGM, .names = "{.col}_FGM"))
CBA.CPhigh.maint.FGM.upper <- CBA.CPhigh.maint.upper %>% mutate(across(contains("AUD"),funFGM2, .names = "{.col}_FGM"))

# select columns needed
CBA.CPlow.maint.FGM.df.upper <- CBA.CPlow.maint.FGM.upper %>% dplyr::select("abate_yr1_AUD_maint_FGM","abate_yr2_AUD_maint_FGM",
                                                                "abate_yr3_AUD_maint_FGM","abate_yr4_AUD_maint_FGM","abate_yr5_AUD_maint_FGM","abate_yr6_AUD_FGM","abate_yr7_AUD_FGM","abate_yr8_AUD_FGM","abate_yr9_AUD_FGM","abate_yr10_AUD_FGM",     
                                                                "abate_yr11_AUD_FGM","abate_yr12_AUD_FGM","abate_yr13_AUD_FGM","abate_yr14_AUD_FGM","abate_yr15_AUD_FGM","abate_yr16_AUD_FGM","abate_yr17_AUD_FGM","abate_yr18_AUD_FGM",     
                                                                "abate_yr19_AUD_FGM","abate_yr20_AUD_FGM","abate_yr21_AUD_FGM","abate_yr22_AUD_FGM","abate_yr23_AUD_FGM","abate_yr24_AUD_FGM","abate_yr25_AUD_FGM") %>% as.data.frame
CBA.CPhigh.maint.FGM.df.upper <- CBA.CPhigh.maint.FGM.upper %>% dplyr::select("abate_yr1_AUD_maint_FGM","abate_yr2_AUD_maint_FGM",
                                                                  "abate_yr3_AUD_maint_FGM","abate_yr4_AUD_maint_FGM","abate_yr5_AUD_maint_FGM","abate_yr6_AUD_FGM","abate_yr7_AUD_FGM","abate_yr8_AUD_FGM","abate_yr9_AUD_FGM","abate_yr10_AUD_FGM",     
                                                                  "abate_yr11_AUD_FGM","abate_yr12_AUD_FGM","abate_yr13_AUD_FGM","abate_yr14_AUD_FGM","abate_yr15_AUD_FGM","abate_yr16_AUD_FGM","abate_yr17_AUD_FGM","abate_yr18_AUD_FGM",     
                                                                  "abate_yr19_AUD_FGM","abate_yr20_AUD_FGM","abate_yr21_AUD_FGM","abate_yr22_AUD_FGM","abate_yr23_AUD_FGM","abate_yr24_AUD_FGM","abate_yr25_AUD_FGM") %>% as.data.frame

##lower
# Annuity GM with average CP across 25 years
CBA.CPlow.lower <- CBA.lower %>% mutate(across(starts_with("abate_yr"), funCP, .names = "{.col}_AUD"))
CBA.CPhigh.lower <- CBA.lower %>% mutate(across(starts_with("abate_yr"), funCPhigh, .names = "{.col}_AUD"))

#mutate to minus off maintenance cost for first five years
CBA.CPlow.maint.lower <- CBA.CPlow.lower %>% mutate(across(ends_with("AUD"), funMaint, .names = "{.col}_maint"))
CBA.CPhigh.maint.lower <- CBA.CPhigh.lower %>% mutate(across(ends_with("AUD"), funMaint2, .names = "{.col}_maint"))

# mutate to minus off FGM (class B and class C) #only have one farm practice
CBA.CPlow.maint.FGM.lower <- CBA.CPlow.maint.lower %>% mutate(across(contains("AUD"), funFGM, .names = "{.col}_FGM"))
CBA.CPhigh.maint.FGM.lower <- CBA.CPhigh.maint.lower %>% mutate(across(contains("AUD"),funFGM2, .names = "{.col}_FGM"))

# select columns needed
CBA.CPlow.maint.FGM.df.lower <- CBA.CPlow.maint.FGM.lower %>% dplyr::select("abate_yr1_AUD_maint_FGM","abate_yr2_AUD_maint_FGM",
                                                                            "abate_yr3_AUD_maint_FGM","abate_yr4_AUD_maint_FGM","abate_yr5_AUD_maint_FGM","abate_yr6_AUD_FGM","abate_yr7_AUD_FGM","abate_yr8_AUD_FGM","abate_yr9_AUD_FGM","abate_yr10_AUD_FGM",     
                                                                            "abate_yr11_AUD_FGM","abate_yr12_AUD_FGM","abate_yr13_AUD_FGM","abate_yr14_AUD_FGM","abate_yr15_AUD_FGM","abate_yr16_AUD_FGM","abate_yr17_AUD_FGM","abate_yr18_AUD_FGM",     
                                                                            "abate_yr19_AUD_FGM","abate_yr20_AUD_FGM","abate_yr21_AUD_FGM","abate_yr22_AUD_FGM","abate_yr23_AUD_FGM","abate_yr24_AUD_FGM","abate_yr25_AUD_FGM") %>% as.data.frame
CBA.CPhigh.maint.FGM.df.lower <- CBA.CPhigh.maint.FGM.lower %>% dplyr::select("abate_yr1_AUD_maint_FGM","abate_yr2_AUD_maint_FGM",
                                                                              "abate_yr3_AUD_maint_FGM","abate_yr4_AUD_maint_FGM","abate_yr5_AUD_maint_FGM","abate_yr6_AUD_FGM","abate_yr7_AUD_FGM","abate_yr8_AUD_FGM","abate_yr9_AUD_FGM","abate_yr10_AUD_FGM",     
                                                                              "abate_yr11_AUD_FGM","abate_yr12_AUD_FGM","abate_yr13_AUD_FGM","abate_yr14_AUD_FGM","abate_yr15_AUD_FGM","abate_yr16_AUD_FGM","abate_yr17_AUD_FGM","abate_yr18_AUD_FGM",     
                                                                              "abate_yr19_AUD_FGM","abate_yr20_AUD_FGM","abate_yr21_AUD_FGM","abate_yr22_AUD_FGM","abate_yr23_AUD_FGM","abate_yr24_AUD_FGM","abate_yr25_AUD_FGM") %>% as.data.frame


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
# NPV (discounted cash flow analysis for for 7 scenarios)

# NPV for 25 years, 1% discount rate, current carbon price, lower rest cost, and median values for carbon parameters (base scenario)
CBA$DCF_25_4_CPlow <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM.df)))  
head(CBA$DCF_25_4_CPlow) # check function
# lower restoration cost
CBA$NPV_25_4_CPlow_RClow <- NPV_C(CBA$DCF_25_4_CPlow, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, current carbon price, and upper rest cost
# upper restoration cost
CBA$NPV_25_4_CPlow_RChigh <- NPV_C(CBA$DCF_25_4_CPlow, c=CBA$rest_high_cost_scale)

# NPV  with 25 years, 4% discount rate, current carbon price and lower rest cost
CBA$DCF_25_7_CPlow <- mapply(npv1, i=0.04, cf=as.data.frame(t(CBA.CPlow.maint.FGM.df)))
# lower restoration cost
CBA$NPV_25_7_CPlow_RClow <- NPV_C(CBA$DCF_25_7_CPlow, c=CBA$rest_low_cost_scale)

# NPV for 25 years, 1% discount rate, higher carbon price and lower rest cost
CBA$DCF_25_4_CPhigh <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPhigh.maint.FGM.df))) 
# lower restoration cost
CBA$NPV_25_4_CPhigh_RClow <- NPV_C(CBA$DCF_25_4_CPhigh, c=CBA$rest_low_cost_scale)

# NPV for 100 years, 1% discount rate, current carbon price, lower rest cost - can't credit beyond 25 years, so don't include this scenario
# get into dataframe
#colnames(CBA.CPlow.maint.FGM)
#CBA.CPlow.maint.FGM.100.df <- CBA.CPlow.maint.FGM %>% dplyr::select(411:415,316:410) %>% as.data.frame
#colnames(CBA.CPlow.maint.FGM.100.df) #check correct columns and that maintenance has been deducted for first five years
#CBA$DCF_100_4_CPlow <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM.100.df))) 
# lower restoration cost
#CBA$NPV_100_4_CPlow_RClow <- NPV_C(CBA$DCF_100_4_CPlow, c=CBA$rest_low_cost_scale)

# NPV for upper 95% CI with 25 years, 1% discount rate, current carbon price, lower rest cost
CBA$DCF_25_4_CPlow_upperCI <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM.df.upper))) 
# lower restoration cost
CBA$NPV_25_4_CPlow_RClow_upperCI <- NPV_C(CBA$DCF_25_4_CPlow_upperCI, c=CBA$rest_low_cost_scale)

# NPV for lower 95% CI with 25 years, 1% discount rate, current carbon price, lower rest cost
CBA$DCF_25_4_CPlow_lowerCI <- mapply(npv1, i=0.01, cf=as.data.frame(t(CBA.CPlow.maint.FGM.df.lower))) 
# lower restoration cost
CBA$NPV_25_4_CPlow_RClow_lowerCI <- NPV_C(CBA$DCF_25_4_CPlow_lowerCI, c=CBA$rest_low_cost_scale)

# save file
write.csv(CBA, file="CBA.csv", row.names=F)
which(CBA$Area_ha>500) #10 large sites


### Explore NPV ###
# Calculate which options are profitable
colnames(CBA)

#calculate number of profitable sites
NPV <- CBA %>% 
  dplyr::select("FID_site", "Area_ha","BASIN_NAME", "NPV_25_4_CPlow_RClow", "NPV_25_4_CPlow_RChigh",  
         "NPV_25_4_CPhigh_RClow", "NPV_25_7_CPlow_RClow", "NPV_25_4_CPlow_RClow_upperCI","NPV_25_4_CPlow_RClow_lowerCI") %>% as.data.frame
colnames(NPV)

# remove CYC no go areas
## No go areas - sites containing known CYC populations (CYC_sites_buffer) 
rest.sites.CYCsites <- read_excel("rsites4_CYCsites_1kmbuf.xls", sheet="rsites4_CYCsites_1kmbuf")
colnames(rest.sites.CYCsites)
dim(rest.sites.CYCsites)
range(rest.sites.CYCsites$FID) #FID is the FID_sites
rest.sites.CYC <- rest.sites.CYCsites %>% dplyr::select(FID_site="FID", CYC="Count_")
colnames(rest.sites.CYC)
# Remove if Count_==1
NPV <- full_join(NPV, rest.sites.CYC, by="FID_site")#join to dataframe
NPV <- subset(NPV, NPV$CYC==0)
dim(NPV)
sum(NPV$Area_ha)

fun <- function(x) {
  length(which(x>0))
}

colnames(NPV)
NPV.prof <- NPV[,-c(1,2,3,10)]
NPV.profit <- apply(NPV.prof,2,fun) %>% as.data.frame
colnames(NPV.profit) <- "profit"
NPV.profit

NPV.profit$prop <- NPV.profit$profit/length(NPV$FID_site)

write.csv(NPV.profit, file="NPV_profitable.csv", row.names=T)

# calculate area of profitable sites across scenarios
NPV.base <- subset(NPV, NPV$NPV_25_4_CPlow_RClow>0)
area.profit.base <- sum(NPV.base$Area_ha)
area.profit.base/sum(NPV$Area_ha) 
NPV.7 <- subset(NPV, NPV$NPV_25_7_CPlow_RClow>0)
area.profit.7 <- sum(NPV.7$Area_ha)
area.profit.7/sum(NPV$Area_ha) 
NPV.CPhigh <- subset(NPV, NPV$NPV_25_4_CPhigh_RClow>0)
area.profit.CPhigh <- sum(NPV.CPhigh$Area_ha)
area.profit.CPhigh/sum(NPV$Area_ha) 
NPV.RChigh <- subset(NPV, NPV$NPV_25_4_CPlow_RChigh>0)
area.profit.RChigh <- sum(NPV.RChigh$Area_ha)
area.profit.RChigh/sum(NPV$Area_ha) 
#NPV.100 <- subset(NPV, NPV$NPV_100_4_CPlow_RClow>0) #exclude 100yr scenario as credit period 5-25 years
#area.profit.100 <- sum(NPV.100$Area_ha)
#area.profit.100/sum(NPV$Area_ha) 
NPV.upper <- subset(NPV, NPV$NPV_25_4_CPlow_RClow_upperCI>0) 
area.profit.upper <- sum(NPV.upper$Area_ha)
area.profit.upper/sum(NPV$Area_ha) 
NPV.lower <- subset(NPV, NPV$NPV_25_4_CPlow_RClow_lowerCI>0) 
area.profit.lower <- sum(NPV.lower$Area_ha)
area.profit.lower/sum(NPV$Area_ha) 

#make barplot
greys6 <- gray.colors(6, start = 0, end = 0.8, gamma = 2.2, alpha = NULL) #greyscale for manuscript (6 shades)
NPV_names<-c("S1:25yr 1% CP57 lowRC", "S2:25yr 1% CP57 highRC", "S3:25yr 1% CP132 lowRC", "S4:25yr 4% CP57 lowRC", "S5:25yr 1% CP57 lowRC upperCI", "S5:25yr 1% CP57 lowRC lowerCI")
tiff("NPV_barplot.tif", units="mm", width=95, height=80, res=800, compression = "lzw")
par(mfrow=c(1,1))
par(mar=c(5,5,1,1), mgp=c(4,0.5,0), tck=-0.01)
barplot(NPV.profit$prop, beside = T, xlab="", cex.names = 0.5, ylim=c(0,1), axis.lty=1,
        ylab= "Proportion of profitable sites", las = 2, cex.lab = 1.0, col="grey40", names.arg = NPV_names, border = NA)
dev.off()

# Determine rankings of NPV
NPV$NPV_25_4_CPlow_RClow_rank <- rank(-NPV$NPV_25_4_CPlow_RClow)
NPV$NPV_25_4_CPhigh_RClow_rank <- rank(-NPV$NPV_25_4_CPhigh_RClow)
NPV$NPV_25_4_CPlow_RChigh_rank <- rank(-NPV$NPV_25_4_CPlow_RChigh)
NPV$NPV_25_7_CPlow_RClow_rank <- rank(-NPV$NPV_25_7_CPlow_RClow)
#NPV$NPV_100_4_CPlow_RClow_rank <- rank(-NPV$NPV_100_4_CPlow_RClow)
NPV$NPV_25_4_CPlow_RClow_upperCI_rank <- rank(-NPV$NPV_25_4_CPlow_RClow_upperCI)
NPV$NPV_25_4_CPlow_RClow_lowerCI_rank <- rank(-NPV$NPV_25_4_CPlow_RClow_lowerCI)

#get best ranking sites
NPV.rank <- NPV %>% 
  select(NPV_25_4_CPlow_RClow_rank, NPV_25_4_CPhigh_RClow_rank, NPV_25_4_CPlow_RChigh_rank, NPV_25_7_CPlow_RClow_rank, NPV_25_4_CPlow_RClow_upperCI_rank, NPV_25_4_CPlow_RClow_lowerCI_rank)
colnames(NPV.rank)
which.top.10 <- function(x) {
  which(x<=10)
}
NPV.top10 <- apply(NPV.rank, 2, which.top.10)
NPV.top10

# Figure - boxplot of NPV and barplot of NPV profit areas per basin - add barplot of NPV profit sites per landuse
head(NPV)
NPV.plot <- NPV[,c(4:9)]
head(NPV.plot) # can't log as some are negative
range <- apply(NPV.plot, 2, range)
med <- apply(NPV.plot, 2, median)
mean <- apply(NPV.plot, 2, mean)

#figure of proportion of profitable sites per basin
NPV.basin <- NPV %>% 
  dplyr::select("FID_site", "Area_ha","BASIN_NAME", "NPV_25_4_CPlow_RClow", "NPV_25_4_CPlow_RChigh",  
         "NPV_25_4_CPhigh_RClow", "NPV_25_7_CPlow_RClow", "NPV_25_4_CPlow_RClow_upperCI", "NPV_25_4_CPlow_RClow_lowerCI") %>% as.data.frame
colnames(NPV.basin)

NPV.profit.basin.S1 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_RClow>0)
NPV.profit.basin.S1.area <- with(NPV.profit.basin.S1, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S1 <- NPV.profit.basin.S1 %>% 
  dplyr::select("NPV_25_4_CPlow_RClow", "BASIN_NAME") 
NPV.profit.basin.S1 <- NPV.profit.basin.S1[,-1]
NPV.profit.basin.S1.freq <- table(NPV.profit.basin.S1) %>% as.matrix
prop.table(NPV.profit.basin.S1.freq,2)

NPV.profit.basin.S2 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_RChigh>0)
NPV.profit.basin.S2.area <- with(NPV.profit.basin.S2, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S2 <- NPV.profit.basin.S2 %>% 
  dplyr::select("NPV_25_4_CPlow_RChigh", "BASIN_NAME") 
NPV.profit.basin.S2 <- NPV.profit.basin.S2[,-1]
NPV.profit.basin.S2.freq <- table(NPV.profit.basin.S2) %>% as.matrix
prop.table(NPV.profit.basin.S2.freq,2) #none

NPV.profit.basin.S3 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPhigh_RClow>0)
NPV.profit.basin.S3.area <- with(NPV.profit.basin.S3, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S3 <- NPV.profit.basin.S3 %>% 
  dplyr::select("NPV_25_4_CPhigh_RClow", "BASIN_NAME") 
NPV.profit.basin.S3 <- NPV.profit.basin.S3[,-1]
NPV.profit.basin.S3.freq <- table(NPV.profit.basin.S3) %>% as.matrix
prop.table(NPV.profit.basin.S3.freq,2) 

NPV.profit.basin.S4 <- subset(NPV.basin, NPV.basin$NPV_25_7_CPlow_RClow>0)
NPV.profit.basin.S4.area <- with(NPV.profit.basin.S4, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S4 <- NPV.profit.basin.S4 %>% 
  dplyr::select("NPV_25_7_CPlow_RClow", "BASIN_NAME") 
NPV.profit.basin.S4 <- NPV.profit.basin.S4[,-1]
NPV.profit.basin.S4.freq <- table(NPV.profit.basin.S4) %>% as.matrix
prop.table(NPV.profit.basin.S4.freq,2) 

#NPV.profit.basin.S5 <- subset(NPV.basin, NPV.basin$NPV_100_4_CPlow_RClow>0)
#NPV.profit.basin.S5.area <- with(NPV.profit.basin.S5, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
#NPV.profit.basin.S5 <- NPV.profit.basin.S5 %>% 
#  dplyr::select("NPV_100_4_CPlow_RClow", "BASIN_NAME") 
#NPV.profit.basin.S5 <- NPV.profit.basin.S5[,-1]
#NPV.profit.basin.S5.freq <- table(NPV.profit.basin.S5) %>% as.matrix
#prop.table(NPV.profit.basin.S5.freq,2)

NPV.profit.basin.S5 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_RClow_upperCI>0)
NPV.profit.basin.S5.area <- with(NPV.profit.basin.S5, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S5 <- NPV.profit.basin.S5 %>% 
  dplyr::select("NPV_25_4_CPlow_RClow_upperCI", "BASIN_NAME") 
NPV.profit.basin.S5 <- NPV.profit.basin.S5[,-1]
NPV.profit.basin.S5.freq <- table(NPV.profit.basin.S5) %>% as.matrix
prop.table(NPV.profit.basin.S5.freq,2)

NPV.profit.basin.S6 <- subset(NPV.basin, NPV.basin$NPV_25_4_CPlow_RClow_lowerCI>0)
NPV.profit.basin.S6.area <- with(NPV.profit.basin.S6, tapply(Area_ha, BASIN_NAME, sum, na.rm=T)) %>% as.data.frame
NPV.profit.basin.S6 <- NPV.profit.basin.S6 %>% 
  dplyr::select("NPV_25_4_CPlow_RClow_lowerCI", "BASIN_NAME") 
NPV.profit.basin.S6 <- NPV.profit.basin.S6[,-1]
NPV.profit.basin.S6.freq <- table(NPV.profit.basin.S6) %>% as.matrix
prop.table(NPV.profit.basin.S6.freq,2)

#join dataframe
NPV.profit.basin.S1.freq <- NPV.profit.basin.S1.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S2.freq <- NPV.profit.basin.S2.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S3.freq <- NPV.profit.basin.S3.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S4.freq <- NPV.profit.basin.S4.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S5.freq <- NPV.profit.basin.S5.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S6.freq <- NPV.profit.basin.S6.freq %>% as.data.frame %>% tibble::rownames_to_column("Basin")

NPV.profit.basin.all <- full_join(NPV.profit.basin.S1.freq, NPV.profit.basin.S2.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S3.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S4.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S5.freq, by="Basin")
NPV.profit.basin.all <- full_join(NPV.profit.basin.all, NPV.profit.basin.S6.freq, by="Basin")

NPV.profit.basin.all <- column_to_rownames(NPV.profit.basin.all, var="Basin")

colnames(NPV.profit.basin.all) <- c("NPV_25_4_CPlow_RClow", "NPV_25_4_CPlow_RChigh", "NPV_25_4_CPhigh_RClow", 
                                     "NPV_25_7_CPlow_RClow", "NPV_25_4_CPlow_RClow_upperCI", "NPV_25_4_CPlow_RClow_lowerCI")
#change NAs to 0
NPV.profit.basin.all[is.na(NPV.profit.basin.all)] <- 0
NPV.profit.basin.all <- NPV.profit.basin.all %>% as.matrix
write.csv(NPV.profit.basin.all, "NPV_profit_basins_all.csv", row.names= T)

#combine area per basin
NPV.profit.basin.S1.area <- NPV.profit.basin.S1.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S2.area <- NPV.profit.basin.S2.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S3.area <- NPV.profit.basin.S3.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S4.area <- NPV.profit.basin.S4.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S5.area <- NPV.profit.basin.S5.area %>% tibble::rownames_to_column("Basin")
NPV.profit.basin.S6.area <- NPV.profit.basin.S6.area %>% tibble::rownames_to_column("Basin")

NPV.profit.basin.area <- full_join(NPV.profit.basin.S1.area, NPV.profit.basin.S2.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S3.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S4.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S5.area, by="Basin")
NPV.profit.basin.area <- full_join(NPV.profit.basin.area, NPV.profit.basin.S6.area, by="Basin")

NPV.profit.basin.area <- column_to_rownames(NPV.profit.basin.area, var="Basin")
colnames(NPV.profit.basin.area) <- c("NPV_25_4_CPlow_RClow", "NPV_25_4_CPlow_RChigh", "NPV_25_4_CPhigh_RClow", 
                                    "NPV_25_7_CPlow_RClow", "NPV_25_4_CPlow_RClow_upperCI", "NPV_25_4_CPlow_RClow_lowerCI")

#change NAs to 0
NPV.profit.basin.area[is.na(NPV.profit.basin.area)] <- 0
NPV.profit.basin.area <- NPV.profit.basin.area %>% as.matrix
write.csv(NPV.profit.basin.area, "NPV_profit_basins_area.csv", row.names= T)

#restorable area per basin
rest.area.basins <- with(NPV, tapply(Area_ha, BASIN_NAME, sum)) %>% as.data.frame
colnames(rest.area.basins) <- "rest_area_ha"
prop.table(rest.area.basins)
range(NPV$NPV_25_4_CPhigh_RClow)

#save dataframes
write.csv(NPV, "NPV.df.csv", row.names = F) #note this is the subset
write.csv(CBA, "CBA.df.csv", row.names = F)


#mean carbon abatement per catchment per ha
range(CBA$abate_CO2eMg_25yrs)
CBA$abate_CO2eMg_25yrs_ha <- CBA$abate_CO2eMg_25yrs/CBA$Area_ha
abate.25yrs.ha.basin.mean <- with(CBA, tapply(abate_CO2eMg_25yrs_ha, BASIN_NAME, mean))
abate.25yrs.ha.basin.length <- with(CBA, tapply(abate_CO2eMg_25yrs_ha, BASIN_NAME, length)) 
abate.25yrs.ha.basin.var <- with(CBA, tapply(abate_CO2eMg_25yrs_ha, BASIN_NAME, var))
abate.25yrs.ha.basin.se <- sqrt(abate.25yrs.ha.basin.var/abate.25yrs.ha.basin.length)
abate.25yrs.ha.basin.upper <- abate.25yrs.ha.basin.mean + abate.25yrs.ha.basin.se
abate.25yrs.ha.basin.lower <- abate.25yrs.ha.basin.mean - abate.25yrs.ha.basin.se

CBA$abate_CO2eMg_yr_ha <- (CBA$abate_CO2eMg_25yrs/25)/CBA$Area_ha
abate.yr.ha.basin.mean <- with(CBA, tapply(abate_CO2eMg_yr_ha, BASIN_NAME, mean))
abate.yr.ha.basin.length <- with(CBA, tapply(abate_CO2eMg_yr_ha, BASIN_NAME, length)) 
abate.yr.ha.basin.var <- with(CBA, tapply(abate_CO2eMg_yr_ha, BASIN_NAME, var))
abate.yr.ha.basin.se <- sqrt(abate.yr.ha.basin.var/abate.yr.ha.basin.length)
abate.yr.ha.basin.upper <- abate.yr.ha.basin.mean + abate.yr.ha.basin.se
abate.yr.ha.basin.lower <- abate.yr.ha.basin.mean - abate.yr.ha.basin.se

abate.yr.ha.mean <- mean(CBA$abate_CO2eMg_yr_ha)
abate.yr.ha.length <- length(CBA$abate_CO2eMg_yr_ha) 
abate.yr.ha.var <- var(CBA$abate_CO2eMg_yr_ha)
abate.yr.ha.se <- sqrt(abate.yr.ha.var/abate.yr.ha.length)

##upper
CBA.upper$abate_CO2eMg_yr_ha <- (CBA.upper$abate_CO2eMg_25yrs/25)/CBA.upper$Area_ha
abate.yr.ha.mean.upper <- mean(CBA.upper$abate_CO2eMg_yr_ha)
abate.yr.ha.length.upper <- length(CBA.upper$abate_CO2eMg_yr_ha) 
abate.yr.ha.var.upper <- var(CBA.upper$abate_CO2eMg_yr_ha)
abate.yr.ha.se.upper <- sqrt(abate.yr.ha.var.upper/abate.yr.ha.length.upper)

##lower
CBA.lower$abate_CO2eMg_yr_ha <- (CBA.lower$abate_CO2eMg_25yrs/25)/CBA.lower$Area_ha
abate.yr.ha.mean.lower <- mean(CBA.lower$abate_CO2eMg_yr_ha)
abate.yr.ha.length.lower <- length(CBA.lower$abate_CO2eMg_yr_ha) 
abate.yr.ha.var.lower <- var(CBA.lower$abate_CO2eMg_yr_ha)
abate.yr.ha.se.lower <- sqrt(abate.yr.ha.var.lower/abate.yr.ha.length.lower)

#standard errors
se <- function(x) sqrt(var(x)/length(x))

#multi-panel fig with restoration area, carbon abatement, NPV scenarios and profitable sites across basins
tiff("fig.rest.carbon.NPV.tif", units="mm", width=190, height=190, res=800, compression = "lzw")
par(mfrow=c(2,2), mar=c(7.2,5,1,1), mgp=c(4,0.3,0), tck=-0.01)
NPV_names
basin_names <- rownames(NPV.profit.basin.all) 
library(RColorBrewer)
basin.palette <- brewer.pal(8, "Paired")
#display.brewer.pal(7, "Paired")

#1 potential restorable area
barplot(rest.area.basins$rest_area_ha, names.arg=rownames(rest.area.basins), cex.names=0.8, cex.lab=1.0, ylab="Potential restoration area", las=2, ylim=c(0,14000))
mtext("a", line=0, side=3, adj=0, cex=1.3)

#2 carbon abatement variation among basins
abate.yr.ha.basin.mean.plot <- abate.yr.ha.basin.mean %>% as.matrix %>% as.vector
abate.yr.ha.basin.upper.plot <- abate.yr.ha.basin.upper %>% as.matrix %>% as.vector
abate.yr.ha.basin.lower.plot <- abate.yr.ha.basin.lower %>% as.matrix %>% as.vector
plot(abate.yr.ha.basin.mean.plot ~ c(1:length(abate.yr.ha.basin.mean.plot)), xlab="", ylab="Mean carbon abatement (Mg CO2-e ha-1 yr-1)", 
     pch=19, xaxt="n", las=2)#, ylim=c(0, 110)) 
arrows(c(1:length(abate.yr.ha.basin.mean.plot)), abate.yr.ha.basin.upper.plot, c(1:length(abate.yr.ha.basin.mean.plot)), abate.yr.ha.basin.lower.plot, length=0.05, angle=90, code=3) ## add standard errors
axis(side=1, at=c(1:length(abate.yr.ha.basin.mean.plot)), labels=rownames(abate.yr.ha.basin.mean), cex.axis=0.8, las=2)  ## add category names to x axis
mtext("b", line=0, side=3, adj=0, cex=1.3)

#3 NPV among scenarios
boxplot(NPV.plot, xlab="", ylab="Net present value (AUD)", las = 2, names = NPV_names, cex.axis=0.7, cex.lab=1.0)
text(1, -200000000, "20518", cex=1) #add median values
text(2, -200000000, "-180569", cex=1)
text(3, -200000000, "110443", cex=1)
text(4, -200000000, "7645", cex=1)
text(5, -200000000, "28664", cex=1)
text(6, -200000000, "19361", cex=1)
#add number of sites - don't need as on next fig
mtext("c", line=0, side=3, adj=0, cex=1.3)

barplot(NPV.profit.basin.area, col=basin.palette, las=2, names.arg=NPV_names, cex.names=0.7, cex.lab=1.0, ylab="Area of profitable sites (ha)", ylim=c(0,14000))
legend(x=0.5, y=12000, legend=basin_names, fill=basin.palette, border=NA, cex=0.8)
mtext("d", line=0, side=3, adj=0, cex=1.3)
dev.off()


# Compare NPV scenarios with dendograms and ordination
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
par(mfrow=c(1,2), mar=c(4,4,1,2))
plot(hc.NPV.jac.t, labels=NPV_names, xlab = "Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("a", line=0, side=3, adj=0, cex=1.5)
plot(NPV.t.mds$points, type='n', xlab='NMDS1', ylab='NMDS2')
ordilabel(NPV.t.mds$points, lab=NPV_names, cex=0.5)
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()

tiff("fig.NPV.dendo.tif", units="mm", width=95, height=95, res=800, compression = "lzw")
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(hc.NPV.jac.t, labels=NPV_names, xlab = "NPV Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("a", line=0, side=3, adj=0, cex=1.5)
dev.off()
#get data for next script 
write.csv(NPV.matrix, file="NPV.matrix.csv", row.names=F) 

