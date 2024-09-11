# Calculates co-benefits of restoration sites and undertakes the cost-effectiveness analysis for Peel-Harvey
# Renee Rossini and Valerie Hagger 16/02/2024

#set working directory 
setwd("R:/NESP115-Q4418/R_working_directory/Peel")

# load packages
library(tidyverse)
library(readxl)

#Read in data on co-benefits analysed in ArcGIS
Raw_data <- read_excel("Raw_data.xls")
View(Raw_data)


# Workflow for cost effectiveness analysis
# read in restoration sites and NPV results
CBA <- read.csv("CBA.df.csv", header=T)
colnames(CBA)


#select columns needed for cost effectiveness analysis

  
#---Restoration Feasibility---
# Peel Harvey is different to Fitzroy - the tidal range is very low so the tidal connectivity is about connection to the ocean via semi-enclosed estuaries.  
# Two factors are combined as an average to give a feasibility score out of 1
# 1) connectivity to the ocean AND 2) the presence and number of barriers or drains.  
# Highest scores go to sites with short distances to an ocean connection AND long areas of barriers or drains.
# In Peel, barries/drains are measured in distance, not a count, so IQR calculated for distance of barrier then assigned probabilities
# based on splits of 50m as per IQR, 0=0.25, 1-50m=0.25, 51-100m=0.5, 101-150m=0.75, >151m=1

#Connectivity to the ocean as a function of success
# no ocean connection = no success because there is no tidal flow to be re-introduced  
# can't have blanks, so "no ocean connection" (0) assigned 1000km as distance and 0.25 as probability  
# then split others based on the IQR of the remaining data

summary(Raw_data$FEAS_BarrierDrain)
summary(Raw_data$FEAS_disttosea_km)

# probabilities applied in spatial analysis and verified on map
Raw_data$FEAS_bars_prob
Raw_data$FEAS_conn_prob

# combine and take average
Raw_data$rest.feas


#---Estimate co-benefits--- 
# Biodiversity benefit
## wetland connectivity - distance to an important wetland. Shorter distance = higher score, therefore reverse
range(Raw_data$BIO_NN_imp_wetland) 
Raw_data$Wetland_dist_scale <- 100-((Raw_data$BIO_NN_imp_wetland/max(Raw_data$BIO_NN_imp_wetland))*100)
range(Raw_data$Wetland_dist_scale) #check data #replace NAs with 0 if required
#higher proportion, closer distance

## Ramsar connectivity - distance to a Ramsar wetland. Shorter distance = higher score, therefore reverse
range(Raw_data$BIO_NN_RAMSAR)
Raw_data$RAMSAR_dist_scale <- 100-((Raw_data$BIO_NN_RAMSAR/max(Raw_data$BIO_NN_RAMSAR))*100)
range(Raw_data$RAMSAR_dist_scale) #check data #replace NAs with 0 if required
#higher proportion, closer distance

## EVNT taxa - number of records within 1000m of site. More records = higher score  
#data is very skewed, max 735 from one site then the rest around 0-150. So log+1
#transform the raw data, then do the scaler
range(Raw_data$EVNT.n) #check data #replace NAs with 0 if required #735 is maximum
hist(Raw_data$EVNT.n)
Log.EVNT.n<-log1p(Raw_data$EVNT.n)
range(Log.EVNT.n) #check data
hist(Log.EVNT.n)
list(Log.EVNT.n)
Raw_data$EVNT.n.log_scale <- ((Log.EVNT.n/max(Log.EVNT.n))*100)
range(Raw_data$EVNT.n.log_scale)
#without logging
EVNT.n<-Raw_data$EVNT.n
Raw_data$ENVT.n_scale <- ((EVNT.n/max(EVNT.n))*100)
range(Raw_data$ENVT.n_scale)

#higher proportion, more number of ENVT records (not more species FYI)

## EPBC threatened community - number of hectares within a 500m buffer of the site mapped as remnant temperate salt marsh.  More area = higher score.  
# Few sites have it so there are lots of 0s, and it is skewed with one site at 8ha then the others <4ha so log
range(Raw_data$EPBCcomm)
sum(Raw_data$EPBCcomm)
Log.EPBC<-log1p(Raw_data$EPBCcomm)
Raw_data$EPBCcomm.log_scale <- ((Log.EPBC/max(Log.EPBC))*100)
range(Raw_data$EPBCcomm.log_scale) #replace NAs with zero if required
#higher proportion, more area in ha

## Patch size
#Use Area_ha of each site
range(Raw_data$Rsite_area)
Raw_data$Rsite_area_scale <- ((Raw_data$Rsite_area/max(Raw_data$Rsite_area))*100)
range(Raw_data$Rsite_area_scale) #check data 
#higher proportion, more area
sum(Raw_data$Rsite_area)

# Coastal fisheries habitat
## 2 indicators because all are on a watercourse so the distance to a major waterway doesn't apply:
# 1) fish habitat area (amount of riverine or floodplain) 
# 2) distance to fish habitat (distance to a CAPAD marine reserve)

## fish habitat areas
range(Raw_data$FISH_hab_for)
Raw_data$Fish_hab_scale <- ((Raw_data$FISH_hab_for/max(Raw_data$FISH_hab_for))*100)
range(Raw_data$Fish_hab_scale) #check data #replace NAs with zero if required
#higher proportion, more fish habitat area.  Good spread across ones that have, then about half have none

## connection to fish habitat in marine reserves
range(Raw_data$FISH_conn_to_CAPAD) #0-52
#replace 0 (no connection with sea) with 100 because very low connectivity
Raw_data$FISH_conn_to_CAPAD[Raw_data$FISH_conn_to_CAPAD==0] <- 100
hist(Raw_data$FISH_conn_to_CAPAD)
list(Raw_data$FISH_conn_to_CAPAD)
Raw_data$CAPAD_dist_scale <-100-((Raw_data$FISH_conn_to_CAPAD/max(na.omit(Raw_data$FISH_conn_to_CAPAD)))*100)
Raw_data$CAPAD_dist_scale #look at it
#higher proportion, closer distance to a coastal opening
#Raw_data$CAPAD_dist_scale[is.na(Raw_data$CAPAD_dist_scale)] <- 0 #give 0 proportion back to NAs


# DIN removal - Denitrification
#For DIN removal you need high residence time, but you need certain flow
# 3 indicators:
# 1) DIN concentrations
# 2) water residence time (the area of riverine or floodplain within the site) 
# 3) flows (hydraulic efficiency) - how far from a connection to the ocean the site is (Flow path distance of the site boundary along any watercourse to the point of opening of its semi-enclosed estuary)
# For closed estuaries, flow path distance would be 0, so need to reverse code this

# in raw data, water residence time = fish habitat area
# flow path distance = FEAS_disttosea_km (also dist_to_sea and FISH_conn_to_CAPAD, but that includes the extra distance from opening to marine PA)
# in semi-enclosed estuaries, sites that are closer to the sea are more likely to have flow because of tides moving in and out and water flowing out the channel

# DIN concentrations of sites
range(Raw_data$TN_perkm)
hist(Raw_data$TN_perkm)
Raw_data$TN_perkmscale <- ((Raw_data$TN_perkm/max(Raw_data$TN_perkm))*100)
range(Raw_data$TN_perkmscale) 

# Water residence time already calculate for fish (FISH.hab_scale)
range(Raw_data$Fish_hab_scale)

# Distance to ocean already calculated for feasiblity 
range(Raw_data$FEAS_disttosea_km)
#replace 0 (no connection with sea) with 100 because very low connectivity
Raw_data$FEAS_disttosea_km[Raw_data$FEAS_disttosea_km==0] <- 100
#check data distribution
range(Raw_data$FEAS_disttosea_km)
hist(Raw_data$FEAS_disttosea_km)
hist(log1p(Raw_data$FEAS_disttosea_km)) #could log
Raw_data$Sea_dist_scale <-100-((Raw_data$FEAS_disttosea_km/max(na.omit(Raw_data$FEAS_disttosea_km)))*100)
range(Raw_data$Sea_dist_scale)
list(Raw_data$FEAS_disttosea_km)
list(Raw_data$Sea_dist_scale)


#attach flood protection
flood <- read.csv("Rsite x flood prot.csv")
colnames(flood)
flood2 <- flood %>% dplyr::select(Rsite_ID="Poly_ID", flood_area="Area.ha", flood_scale="Scaler")
Raw_data <- full_join(Raw_data, flood2, by="Rsite_ID")
list(Raw_data$flood_scale) #only did flood model extent for 43 sites, therefore convert NAs to 0 for subset
list(Raw_data$flood_area)
Raw_data$flood_area[is.na(Raw_data$flood_area)] <- 0
Raw_data$flood_scale[is.na(Raw_data$flood_scale)] <- 0
range(Raw_data$flood_scale) #check
range(Raw_data$flood_area) #check

#---S7 Cost effectiveness analysis ---
# Calculate cobenefits (B)
# B = sum (Bn*Wn)
# total indicators: biodiversity (5), fisheries (2), DIN (1), flood (0) = 8

# first do some pairs plots do look for collinearity/trade-offs between functions
Raw_ind <- Raw_data %>% dplyr::select(BIO_NN_imp_wetland, BIO_NN_RAMSAR, EVNT.n, EPBCcomm, Rsite_area, #biodiversity
                                      FISH_hab_for, FISH_conn_to_CAPAD, #fisheries
                                      TN_perkm, FISH_hab_for, FEAS_disttosea_km, #DIN removal
                                      flood_area)
Scaled_ind <- Raw_data %>% dplyr::select(Wetland_dist_scale, RAMSAR_dist_scale, EVNT.n_scale, EPBCcomm.log_scale, Rsite_area_scale, #biodiversity
                                         Fish_hab_scale, CAPAD_dist_scale, #fisheries
                                         TN_perkmscale, Fish_hab_scale, Sea_dist_scale, #DIN removal
                                         flood_scale) #flood (inland only)
pairs(Raw_ind)
pairs(Scaled_ind)

# Correlations
#RAMSAR and important wetland nearest neighbour are strongly correlated
#Rsite area and area of fish habitat have a high degree of correlation
#Distance to sea and CAPAD marine reserve have a high degree of correlation

#don't use rsite area as correlated

##Weighted co-benefits
# difficult for stakeholders to assign weightings because they are all biased in particular areas (i.e. biodiversity, cultural heritage, carbon)

# equal weighting of ecosystem services - BD, fish, water, flood each 0.25 (should sum to 1).  There is no flood for Peel Harvey so re-assigned to 0.3333 each

# then divide indicator weightings among service weightings - BD (0.33/5), fish (0.33/2), DIN (0.33/2)
Raw_data$ES_equal <- (Raw_data$Wetland_dist_scale*(0.25/4)) + (Raw_data$RAMSAR_dist_scale*(0.25/4)) + (Raw_data$EVNT.n.log_scale*(0.25/4)) + (Raw_data$EPBCcomm.log_scale*(0.25/4)) + #biodiversity scaled indicators
  (Raw_data$Fish_hab_scale*(0.25/2)) + (Raw_data$CAPAD_dist_scale*(0.25/2)) + #fisheries scaled indicators
  (Raw_data$TN_perkmscale*(0.25/3)) + (Raw_data$Fish_hab_scale*(0.25/3)) + (Raw_data$Sea_dist_scale*(0.25/3)) + #DIN removal scaled indicators
  (Raw_data$flood_scale*(0.25/1))

# each ES high (0.7) with 0.1 each to others = 1
Raw_data$ES_BD <- (Raw_data$Wetland_dist_scale*(0.7/4)) + (Raw_data$RAMSAR_dist_scale*(0.7/4)) + (Raw_data$EVNT.n.log_scale*(0.7/4)) + (Raw_data$EPBCcomm.log_scale*(0.7/4)) + #biodiversity scaled indicators
  (Raw_data$Fish_hab_scale*(0.1/2)) + (Raw_data$CAPAD_dist_scale*(0.1/2)) + #fisheries scaled indicators
  (Raw_data$TN_perkmscale*(0.1/3)) + (Raw_data$Fish_hab_scale*(0.1/3)) + (Raw_data$Sea_dist_scale*(0.1/3)) + #DIN removal scaled indicators
  (Raw_data$flood_scale*(0.1/1))

Raw_data$ES_fish <- (Raw_data$Wetland_dist_scale*(0.1/4)) + (Raw_data$RAMSAR_dist_scale*(0.1/4)) + (Raw_data$EVNT.n.log_scale*(0.1/4)) + (Raw_data$EPBCcomm.log_scale*(0.1/4)) + #biodiversity scaled indicators
  (Raw_data$Fish_hab_scale*(0.7/2)) + (Raw_data$CAPAD_dist_scale*(0.7/2)) + #fisheries scaled indicators
  (Raw_data$TN_perkmscale*(0.1/3)) + (Raw_data$Fish_hab_scale*(0.1/3)) + (Raw_data$Sea_dist_scale*(0.1/3)) + #DIN removal scaled indicators
  (Raw_data$flood_scale*(0.1/1))

Raw_data$ES_DIN <- (Raw_data$Wetland_dist_scale*(0.1/4)) + (Raw_data$RAMSAR_dist_scale*(0.1/4)) + (Raw_data$EVNT.n.log_scale*(0.1/4)) + (Raw_data$EPBCcomm.log_scale*(0.1/4)) + #biodiversity scaled indicators
  (Raw_data$Fish_hab_scale*(0.1/2)) + (Raw_data$CAPAD_dist_scale*(0.1/2)) + #fisheries scaled indicators
  (Raw_data$TN_perkmscale*(0.7/3)) + (Raw_data$Fish_hab_scale*(0.7/3)) + (Raw_data$Sea_dist_scale*(0.7/3)) + #DIN removal scaled indicators
  (Raw_data$flood_scale*(0.1/1))

Raw_data$ES_flood <- (Raw_data$Wetland_dist_scale*(0.1/4)) + (Raw_data$RAMSAR_dist_scale*(0.1/4)) + (Raw_data$EVNT.n.log_scale*(0.1/4)) + (Raw_data$EPBCcomm.log_scale*(0.1/4)) + #biodiversity scaled indicators
  (Raw_data$Fish_hab_scale*(0.1/2)) + (Raw_data$CAPAD_dist_scale*(0.1/2)) + #fisheries scaled indicators
  (Raw_data$TN_perkmscale*(0.1/3)) + (Raw_data$Fish_hab_scale*(0.1/3)) + (Raw_data$Sea_dist_scale*(0.1/3)) + #DIN removal scaled indicators
  (Raw_data$flood_scale*(0.7/1))

range(Raw_data$ES_equal)
range(Raw_data$ES_BD)
range(Raw_data$ES_fish)
range(Raw_data$ES_DIN)
range(Raw_data$ES_flood)

# calculate cost-effectiveness considering NPV, restoration feasibility (F) and co-benefits/ecosystem services (ES)
# relative indicator as to which site is the most effective
# loss: least cost per unit of outcome 
# gain: most profit AND unit of outcome

# join feasibility and co-benefits into NPV dataframe

# Peel Harvey site differences
# On doing the CE analysis sites were cut down from 71 to 43 but the new export of sites and their associated data did not use the Poly_ID from the original dataframe
# Before progressing, did a re-match in QGIS to make sure NPV data matches co-benefit data
# The co-benefit ID for all sites is Poly_ID = Rsite_ID, the NPV ID is FID_site = Phoebe_ID

#subset sites that were removed
# Read in match of Poly ID with Phoebe_ID 
ID <- read_excel("C:/Users/uqvhagge/OneDrive - The University of Queensland/Documents/NESP/R_working_directory/Peel/rsites_Peel_PolyID_PhoebeID.xls") 
head(ID)
ID <- ID[,-1] #remove FID column

# join to raw_data
colnames(Raw_data)[colnames(Raw_data)=="Rsite_ID"] <- "Poly_ID" #change Rsite_ID to Poly_ID
colnames(Raw_data)
Raw_data2 <- full_join(Raw_data, ID, by="Poly_ID")
colnames(Raw_data2) #check join
list(Raw_data2$Rsite_area)
list(Raw_data2$area) #check correct by matching rsite areas from both datasets

#remove 
Raw_data_sub <- subset(Raw_data2, Raw_data2$Phoebe_ID>0)
list(Raw_data_sub$Phoebe_ID)
length(Raw_data_sub$Phoebe_ID) #43 sites


#Re-label to match Val's function codes
  #Do it for both FGM so this run is for any FGM1 data
#join co-benefits and NPV dataframes
NPV <- read.csv("NPV.df.csv", header=T)
list(NPV$FID_site) #0-42
list(Raw_data_sub$Phoebe_ID) 
#need to make the site IDs consistent first
#relabel 1-43
NPV$FID_site <- c(1:43)
#rename to FID_site
colnames(Raw_data_sub)[colnames(Raw_data_sub)=="Phoebe_ID"] <- "FID_site"
list(Raw_data_sub$FID_site)
list(Raw_data_sub$Poly_ID)
list(NPV$FID_site)
#join subset sites
CE <- full_join(NPV, Raw_data_sub, by="FID_site")
colnames(CE)
#check restoration areas
Raw_data_sub$Rsite_area
CE$Rsite_area #this is the area column from Raw_data_sub
CE$restor_ha #this is the area column from NPV - should match up
head(CE)

write.csv(CE, "CE.df.csv", row.names= T)

list(CE$NPV_25_4_CPlow_FGM2_RClow)
table(CE$NPV_25_4_CPlow_FGM2_RClow>0) #many positive
list(CE$NPV_25_4_CPhigh_FGM2_RClow)
table(CE$NPV_25_4_CPhigh_FGM2_RClow>0) #nearly all positive

#site sizes
range(Raw_data_sub$Rsite_area)
dim(subset(Raw_data_sub, Raw_data_sub$Rsite_area<30))
dim(subset(Raw_data_sub, Raw_data_sub$Rsite_area>=30))
## create function
#most effective per $ spent/earned

#loss - expected cost per % cobenefits (ES)
CE.fun.loss <- function(NPV, ES, F) {
  NPV / (ES * F)
}
#gain - expected profit with % cobenefits (ES) and feasibility (F)
CE.fun.profit <- function(NPV, ES, F) {
  NPV * (ES * F)
}

# CE of 2 scenarios - base and higher carbon price
# CE using NPV_25_4_FGM2_CPlow_RClow
##equal weighting
CE.equal <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_FGM2_RClow, ES_equal, rest.feas) 
CE.equal.prof <- subset(CE.equal, CE.equal$NPV_25_4_CPlow_FGM2_RClow>0) #38
CE.equal.loss <- subset(CE.equal, CE.equal$NPV_25_4_CPlow_FGM2_RClow<=0) #5
CE.equal.prof$CE_equal <- CE.fun.profit(NPV=CE.equal.prof$NPV_25_4_CPlow_FGM2_RClow, ES=CE.equal.prof$ES_equal, F=CE.equal.prof$rest.feas)
CE.equal.loss$CE_equal <- CE.fun.loss(NPV=CE.equal.loss$NPV_25_4_CPlow_FGM2_RClow, ES=CE.equal.loss$ES_equal, F=CE.equal.loss$rest.feas)
CE.equal.prof1 <- CE.equal.prof[,-c(2:4)]
CE.equal.loss1 <- CE.equal.loss[,-c(2:4)]
#CE.equal2 <- data.frame(CE.equal.loss1, CE.equal.prof1,FID_site) 
CE2 <- right_join(CE, CE.equal.prof1, by="FID_site") #only rank NPV profit sites
CE2$CE_equal_rank <- rank(-CE2$CE_equal) #rank from 1 (highest score) to lowest score (38)
dim(CE2) #check number of sites, less prof sites (38)

##biodiversity high weighting
CE.BD <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_FGM2_RClow, ES_BD, rest.feas) 
CE.BD.prof <- subset(CE.BD, CE.BD$NPV_25_4_CPlow_FGM2_RClow>0) #38
CE.BD.loss <- subset(CE.BD, CE.BD$NPV_25_4_CPlow_FGM2_RClow<=0) #5
CE.BD.prof$CE_BD <- CE.fun.profit(NPV=CE.BD.prof$NPV_25_4_CPlow_FGM2_RClow, ES=CE.BD.prof$ES_BD, F=CE.BD.prof$rest.feas)
CE.BD.loss$CE_BD <- CE.fun.loss(NPV=CE.BD.loss$NPV_25_4_CPlow_FGM2_RClow, ES=CE.BD.loss$ES_BD, F=CE.BD.loss$rest.feas)
CE.BD.prof1 <- CE.BD.prof[,-c(2:4)]
CE.BD.loss1 <- CE.BD.loss[,-c(2:4)]
#CE.BD2 <- bind_rows(CE.BD.loss1, CE.BD.prof1) %>% as.data.frame
CE2 <- right_join(CE2, CE.BD.prof1, by="FID_site")%>% as.data.frame
CE2$CE_BD_rank <- rank(-CE2$CE_BD)
dim(CE2) #check number of sites

##fisheries high weighting
CE.fish <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_FGM2_RClow, ES_fish, rest.feas) 
CE.fish.prof <- subset(CE.fish, CE.fish$NPV_25_4_CPlow_FGM2_RClow>0) 
CE.fish.loss <- subset(CE.fish, CE.fish$NPV_25_4_CPlow_FGM2_RClow<=0)
CE.fish.prof$CE_fish <- CE.fun.profit(NPV=CE.fish.prof$NPV_25_4_CPlow_FGM2_RClow, ES=CE.fish.prof$ES_fish, F=CE.fish.prof$rest.feas)
CE.fish.loss$CE_fish <- CE.fun.loss(NPV=CE.fish.loss$NPV_25_4_CPlow_FGM2_RClow, ES=CE.fish.loss$ES_fish, F=CE.fish.loss$rest.feas)
CE.fish.prof1 <- CE.fish.prof[,-c(2:4)]
CE.fish.loss1 <- CE.fish.loss[,-c(2:4)]
#CE.fish2 <- bind_rows(CE.fish.loss1, CE.fish.prof1) %>% as.data.frame
CE2 <- right_join(CE2, CE.fish.prof1, by="FID_site")%>% as.data.frame
CE2$CE_fish_rank <- rank(-CE2$CE_fish)
dim(CE2)

##DIN removal high weighting
CE.DIN <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_FGM2_RClow, ES_DIN, rest.feas) 
CE.DIN.prof <- subset(CE.DIN, CE.DIN$NPV_25_4_CPlow_FGM2_RClow>0)
CE.DIN.loss <- subset(CE.DIN, CE.DIN$NPV_25_4_CPlow_FGM2_RClow<=0)
CE.DIN.prof$CE_DIN <- CE.fun.profit(NPV=CE.DIN.prof$NPV_25_4_CPlow_FGM2_RClow, ES=CE.DIN.prof$ES_DIN, F=CE.DIN.prof$rest.feas)
CE.DIN.loss$CE_DIN <- CE.fun.loss(NPV=CE.DIN.loss$NPV_25_4_CPlow_FGM2_RClow, ES=CE.DIN.loss$ES_DIN, F=CE.DIN.loss$rest.feas)
CE.DIN.prof1 <- CE.DIN.prof[,-c(2:4)]
CE.DIN.loss1 <- CE.DIN.loss[,-c(2:4)]
#CE.DIN2 <- bind_rows(CE.DIN.loss1, CE.DIN.prof1) %>% as.data.frame
CE2 <- right_join(CE2, CE.DIN.prof1, by="FID_site")%>% as.data.frame
CE2$CE_DIN_rank <- rank(-CE2$CE_DIN)
head(CE2)

##flood mitigation high weighting
CE.flood <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_FGM2_RClow, ES_flood, rest.feas) 
CE.flood.prof <- subset(CE.flood, CE.flood$NPV_25_4_CPlow_FGM2_RClow>0)
CE.flood.loss <- subset(CE.flood, CE.flood$NPV_25_4_CPlow_FGM2_RClow<=0)
CE.flood.prof$CE_flood <- CE.fun.profit(NPV=CE.flood.prof$NPV_25_4_CPlow_FGM2_RClow, ES=CE.flood.prof$ES_flood, F=CE.flood.prof$rest.feas)
CE.flood.loss$CE_flood <- CE.fun.loss(NPV=CE.flood.loss$NPV_25_4_CPlow_FGM2_RClow, ES=CE.flood.loss$ES_flood, F=CE.flood.loss$rest.feas)
CE.flood.prof1 <- CE.flood.prof[,-c(2:4)]
CE.flood.loss1 <- CE.flood.loss[,-c(2:4)]
#CE.DIN2 <- bind_rows(CE.DIN.loss1, CE.DIN.prof1) %>% as.data.frame
CE2 <- right_join(CE2, CE.flood.prof1, by="FID_site")%>% as.data.frame
CE2$CE_flood_rank <- rank(-CE2$CE_flood)
dim(CE2)

# CE using NPV_25_4_CPhigh_FGM2_RClow (scenario 4 - higher carbon price)
##equal weighting
CE.equal.CP <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_FGM2_RClow, ES_equal, rest.feas) 
CE.equal.CP.prof <- subset(CE.equal.CP, CE.equal.CP$NPV_25_4_CPhigh_FGM2_RClow>0) #42
CE.equal.CP.loss <- subset(CE.equal.CP, CE.equal.CP$NPV_25_4_CPhigh_FGM2_RClow<=0) #1
CE.equal.CP.prof$CE_equal_CPhigh <- CE.fun.profit(NPV=CE.equal.CP.prof$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.equal.CP.prof$ES_equal, F=CE.equal.CP.prof$rest.feas)
CE.equal.CP.loss$CE_equal_CPhigh <- CE.fun.loss(NPV=CE.equal.CP.loss$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.equal.CP.loss$ES_equal, F=CE.equal.CP.loss$rest.feas)
CE.equal.CP.prof2 <- CE.equal.CP.prof[,-c(2:4)]
CE.equal.CP.loss2 <- CE.equal.CP.loss[,-c(2:4)]
#CE.equal.CP2 <- bind_rows(CE.equal.CP.loss2, CE.equal.CP.prof2) %>% as.data.frame
CE3 <- right_join(CE, CE.equal.CP.prof2, by="FID_site")%>% as.data.frame
CE3$CE_equal_CPhigh_rank <- rank(-CE3$CE_equal_CPhigh)
dim(CE3) #check - 42 prof sites

##biodiversity high weighting
CE.BD.CP <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_FGM2_RClow, ES_BD, rest.feas) 
CE.BD.CP.prof <- subset(CE.BD.CP, CE.BD.CP$NPV_25_4_CPhigh_FGM2_RClow>0)
CE.BD.CP.loss <- subset(CE.BD.CP, CE.BD.CP$NPV_25_4_CPhigh_FGM2_RClow<=0)
CE.BD.CP.prof$CE_BD_CPhigh <- CE.fun.profit(NPV=CE.BD.CP.prof$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.BD.CP.prof$ES_BD, F=CE.BD.CP.prof$rest.feas)
CE.BD.CP.loss$CE_BD_CPhigh <- CE.fun.loss(NPV=CE.BD.CP.loss$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.BD.CP.loss$ES_BD, F=CE.BD.CP.loss$rest.feas)
CE.BD.CP.prof2 <- CE.BD.CP.prof[,-c(2:4)]
CE.BD.CP.loss2 <- CE.BD.CP.loss[,-c(2:4)]
#CE.BD.CP2 <- bind_rows(CE.BD.CP.loss2, CE.BD.CP.prof2) %>% as.data.frame
CE3 <- right_join(CE3, CE.BD.CP.prof2, by="FID_site")%>% as.data.frame
CE3$CE_BD_CPhigh_rank <- rank(-CE3$CE_BD_CPhigh)
dim(CE3)

##fisheries high weighting
CE.fish.CP <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_FGM2_RClow, ES_fish, rest.feas) 
CE.fish.CP.prof <- subset(CE.fish.CP, CE.fish.CP$NPV_25_4_CPhigh_FGM2_RClow>0)
CE.fish.CP.loss <- subset(CE.fish.CP, CE.fish.CP$NPV_25_4_CPhigh_FGM2_RClow<=0)
CE.fish.CP.prof$CE_fish_CPhigh <- CE.fun.profit(NPV=CE.fish.CP.prof$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.fish.CP.prof$ES_fish, F=CE.fish.CP.prof$rest.feas)
CE.fish.CP.loss$CE_fish_CPhigh <- CE.fun.loss(NPV=CE.fish.CP.loss$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.fish.CP.loss$ES_fish, F=CE.fish.CP.loss$rest.feas)
CE.fish.CP.prof2 <- CE.fish.CP.prof[,-c(2:4)]
CE.fish.CP.loss2 <- CE.fish.CP.loss[,-c(2:4)]
#CE.fish.CP2 <- bind_rows(CE.fish.CP.loss2, CE.fish.CP.prof2) %>% as.data.frame
CE3 <- right_join(CE3, CE.fish.CP.prof2, by="FID_site")%>% as.data.frame
CE3$CE_fish_CPhigh_rank <- rank(-CE3$CE_fish_CPhigh)
dim(CE3)

##DIN removal high weighting
CE.DIN.CP <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_FGM2_RClow, ES_DIN, rest.feas) 
CE.DIN.CP.prof <- subset(CE.DIN.CP, CE.DIN.CP$NPV_25_4_CPhigh_FGM2_RClow>0)
CE.DIN.CP.loss <- subset(CE.DIN.CP, CE.DIN.CP$NPV_25_4_CPhigh_FGM2_RClow<=0)
CE.DIN.CP.prof$CE_DIN_CPhigh <- CE.fun.profit(NPV=CE.DIN.CP.prof$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.DIN.CP.prof$ES_DIN, F=CE.DIN.CP.prof$rest.feas)
CE.DIN.CP.loss$CE_DIN_CPhigh <- CE.fun.loss(NPV=CE.DIN.CP.loss$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.DIN.CP.loss$ES_DIN, F=CE.DIN.CP.loss$rest.feas)
CE.DIN.CP.prof2 <- CE.DIN.CP.prof[,-c(2:4)]
CE.DIN.CP.loss2 <- CE.DIN.CP.loss[,-c(2:4)]
#CE.DIN.CP2 <- bind_rows(CE.DIN.CP.loss2, CE.DIN.CP.prof2) %>% as.data.frame
CE3 <- right_join(CE3, CE.DIN.CP.prof2, by="FID_site")%>% as.data.frame
CE3$CE_DIN_CPhigh_rank <- rank(-CE3$CE_DIN_CPhigh)
dim(CE3)

##flood mitigation high weighting
CE.flood.CP <- CE %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_FGM2_RClow, ES_flood, rest.feas) 
CE.flood.CP.prof <- subset(CE.flood.CP, CE.flood.CP$NPV_25_4_CPhigh_FGM2_RClow>0)
CE.flood.CP.loss <- subset(CE.flood.CP, CE.flood.CP$NPV_25_4_CPhigh_FGM2_RClow<=0)
CE.flood.CP.prof$CE_flood_CPhigh <- CE.fun.profit(NPV=CE.flood.CP.prof$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.flood.CP.prof$ES_flood, F=CE.flood.CP.prof$rest.feas)
CE.flood.CP.loss$CE_flood_CPhigh <- CE.fun.loss(NPV=CE.flood.CP.loss$NPV_25_4_CPhigh_FGM2_RClow, ES=CE.flood.CP.loss$ES_flood, F=CE.flood.CP.loss$rest.feas)
CE.flood.CP.prof2 <- CE.flood.CP.prof[,-c(2:4)]
CE.flood.CP.loss2 <- CE.flood.CP.loss[,-c(2:4)]
CE3 <- right_join(CE3, CE.flood.CP.prof2, by="FID_site")%>% as.data.frame
CE3$CE_flood_CPhigh_rank <- rank(-CE3$CE_flood_CPhigh)
dim(CE3)

# Comparison of priority approaches
#do subset of highest rankings for each priority approach and calculate benefits
options(scipen = 999)
## carbon abatement
colnames(CBA) #need to take from CBA dataframe
c.abate <- CBA %>% dplyr::select(FID_site, abate_CO2eMg_25yrs)
CE2 <- left_join(CE2, c.abate, by="FID_site")
CE3 <- left_join(CE3, c.abate, by="FID_site")
CE2$abate_CO2eMg_25yrs_rank <- rank(-CE2$abate_CO2eMg_25yrs) #from highest to lowest
which.max(CE2$abate_CO2eMg_25yrs) #matches to rank 1 - row 35
which.min(CE2$abate_CO2eMg_25yrs_rank)
C10 <- subset(CE2, CE2$abate_CO2eMg_25yrs_rank<=10)
C10.2 <- C10 %>% dplyr::select(restor_ha, NPV="NPV_25_4_CPlow_FGM2_RClow", abate_CO2eMg_25yrs, rest.feas, ES_equal)
C10.sum <- apply(C10.2, 2, sum) %>% as.data.frame
colnames(C10.sum)[1]<-"carbon_abate_25yrs" 
C10.ranks <- with(C10, data.frame(FID_site, abate_CO2eMg_25yrs_rank))#, BASIN_NAME))
C10.order <- C10.ranks[order(C10.ranks$abate_CO2eMg_25yrs_rank),]
mean(C10.2$rest.feas)

## ecosystem services equal
CE2$ES_equal_rank <- rank(-CE2$ES_equal) #from highest to lowest
which.min(CE2$ES_equal) #matches to rank 1
which.max(CE2$ES_equal_rank)
ES10 <- subset(CE2, CE2$ES_equal_rank<=10)
ES10.2 <- ES10 %>% dplyr::select(restor_ha, NPV="NPV_25_4_CPlow_FGM2_RClow", abate_CO2eMg_25yrs, rest.feas, ES_equal)
ES10.sum <- apply(ES10.2, 2, sum) %>% as.data.frame
colnames(ES10.sum)[1]<-"ES_equal" 
ES10.ranks <- with(ES10, data.frame(FID_site, ES_equal_rank))#, BASIN_NAME))
ES10.order <- ES10.ranks[order(ES10.ranks$ES_equal_rank),]
mean(ES10.2$rest.feas)

## NPV_25_4_CPlow_FGM2_RClow
CE2$NPV_25_4_CPlow_FGM2_RClow_rank <- rank(-CE2$NPV_25_4_CPlow_FGM2_RClow) #from highest to lowest
which.min(CE2$NPV_25_4_CPlow_FGM2_RClow_rank) #matches to rank 1
which.max(CE2$NPV_25_4_CPlow_FGM2_RClow)
NPV10 <- subset(CE2, CE2$NPV_25_4_CPlow_FGM2_RClow_rank<=10)
NPV10.2 <- NPV10 %>% dplyr::select(restor_ha, NPV="NPV_25_4_CPlow_FGM2_RClow", abate_CO2eMg_25yrs, rest.feas, ES_equal)
NPV10.sum <- apply(NPV10.2, 2, sum) %>% as.data.frame
colnames(NPV10.sum)[1]<-"NPV_25_4_CPlow_FGM2_RClow" 
NPV10.ranks <- with(NPV10, data.frame(FID_site, NPV_25_4_CPlow_FGM2_RClow_rank))#, BASIN_NAME))
NPV10.order <- NPV10.ranks[order(NPV10.ranks$NPV_25_4_CPlow_FGM2_RClow_rank),]

## NPV_25_4_CPhigh_FGM2_RClow - use CE3 as more sites profitable
CE3$NPV_25_4_CPhigh_FGM2_RClow_rank <- rank(-CE3$NPV_25_4_CPhigh_FGM2_RClow)
which.min(CE3$NPV_25_4_CPhigh_FGM2_RClow_rank) #matches to rank 1
which.max(CE3$NPV_25_4_CPhigh_FGM2_RClow)
NPV10.CPhigh <- subset(CE3, CE3$NPV_25_4_CPhigh_FGM2_RClow_rank<=10)
NPV10.CPhigh.2 <- NPV10.CPhigh %>% dplyr::select(restor_ha, NPV="NPV_25_4_CPhigh_FGM2_RClow", abate_CO2eMg_25yrs, rest.feas, ES_equal)
NPV10.CPhigh.sum <- apply(NPV10.CPhigh.2, 2, sum) %>% as.data.frame
colnames(NPV10.CPhigh.sum)[1]<-"NPV_25_4_CPhigh_FGM2_RClow" 
NPV10.CPhigh.ranks <- with(NPV10.CPhigh, data.frame(FID_site, NPV_25_4_CPhigh_FGM2_RClow_rank))#, BASIN_NAME))
NPV10.CPhigh.order <- NPV10.CPhigh.ranks[order(NPV10.CPhigh.ranks$NPV_25_4_CPhigh_FGM2_RClow_rank),]

## CE with ES equal CP low
CE.equal10 <- subset(CE2, CE2$CE_equal_rank<=10)
CE.equal10.2 <- CE.equal10 %>% dplyr::select(restor_ha, NPV="NPV_25_4_CPlow_FGM2_RClow", abate_CO2eMg_25yrs, rest.feas, ES_equal)
CE.equal10.sum <- apply(CE.equal10.2, 2, sum) %>% as.data.frame
colnames(CE.equal10.sum)[1] <- "CE_equal"
CE.equal10.ranks <- with(CE.equal10, data.frame(FID_site, CE_equal_rank))#, BASIN_NAME))
CE.equal10.order <- CE.equal10.ranks[order(CE.equal10.ranks$CE_equal_rank),]

## CE with ES equal CP high
CE.equal10.CPhigh <- subset(CE3, CE3$CE_equal_CPhigh_rank<=10)
CE.equal10.CPhigh.2 <- CE.equal10.CPhigh %>% dplyr::select(restor_ha, NPV="NPV_25_4_CPhigh_FGM2_RClow", abate_CO2eMg_25yrs, rest.feas, ES_equal)
CE.equal10.CPhigh.sum <- apply(CE.equal10.CPhigh.2, 2, sum) %>% as.data.frame
colnames(CE.equal10.CPhigh.sum)[1] <- "CE_equal_CPhigh"
CE.equal10.CPhigh.ranks <- with(CE.equal10.CPhigh, data.frame(FID_site, CE_equal_CPhigh_rank))#, BASIN_NAME))
CE.equal10.CPhigh.order <- CE.equal10.CPhigh.ranks[order(CE.equal10.CPhigh.ranks$CE_equal_CPhigh_rank),]


## combine tables
benefits.table <- cbind(C10.sum, ES10.sum, NPV10.sum, NPV10.CPhigh.sum, CE.equal10.sum, CE.equal10.CPhigh.sum, 
                        rownames=T)

colnames(benefits.table) <- c("C10", "B10","NPV10","NPV10.CPhigh", "CEequal10","CEequal10.CPhigh")
write.csv(benefits.table, file="benefits.top10.sum.csv", row.names=T)

ranks.table <- cbind(C10.order, ES10.order, NPV10.order, NPV10.CPhigh.order, CE.equal10.order, CE.equal10.CPhigh.order)
write.csv(ranks.table, file="benefits.top10.ranks.csv", row.names=F)


#Plot NPV vs CE
tiff("NVP.CE.fig.tif", units="mm", width=120, height=120, res=800, compression = "lzw")
par(mfrow=c(2,1), mar=c(5,5,1,2))
with(CE2, plot(CE_equal_rank ~ NPV_25_4_CPlow_FGM2_RClow_rank, xlab = "NPV site rank (S1)", ylab="CE site rank (S1)", cex.lab=1))
mtext("a", line=0, side=3, adj=0, cex=1.5)
with(CE3, plot(CE_equal_CPhigh_rank ~ NPV_25_4_CPhigh_FGM2_RClow_rank, xlab = "NPV site rank (S3)", ylab="CE site rank (S3)", cex.lab=1))
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()


#save to do cluster analysis in figures
write.csv(CE2, file="CE2.df.CPlow.csv", row.names = T)
write.csv(CE3, file="CE3.df.CPhigh.csv", row.names = T)


# Cluster analysis and nMDS
# Create matrix
# Join variables using CE scenario 
CE2.matrix <- CE2 %>% dplyr::select(FID_site, CE_equal_rank, CE_BD_rank, CE_fish_rank, CE_DIN_rank, CE_flood_rank) 
CE3.matrix <- CE3 %>% 
  dplyr::select(FID_site, "CE_equal_CPhigh_rank", "CE_BD_CPhigh_rank", "CE_fish_CPhigh_rank", "CE_DIN_CPhigh_rank", "CE_flood_CPhigh_rank" ) 

CE.matrix <- left_join(CE2.matrix, CE3.matrix, by="FID_site") %>% as.matrix
dim(CE.matrix)
head(CE.matrix)
CE.matrix <- CE.matrix[,-1]
rownames(CE.matrix) <- c(1:38) #add row names for FID sites

#However have different number of sites
#So take dataset using CP high scenario
CE3.matrix <- CE3.matrix[,-1] %>% as.matrix
rownames(CE3.matrix) <- c(1:42) #add row names for FID sites

# create dissimilarity matrix - all columns add to 1
library(vegan)
CE.d.matrix <- prop.table(CE3.matrix, margin=2)
apply(CE.d.matrix, 2, sum) #all add one
CE.d.matrix.t <- t(CE.d.matrix)

# dissimiliarity matrix with bray curtis
dist.CE.jac <- vegdist(CE.d.matrix, method="bray") #by site
dist.CE.jac.t <- vegdist(CE.d.matrix.t, method="bray") #by scenario

#construct the dendogram by scenario
hc.CE.jac.t <- hclust(dist.CE.jac.t, method="complete")  
head(hc.CE.jac.t) # to identify order of labels

#Construct the nMDS ordination by scenario
CE.t.mds <- metaMDS(dist.CE.jac.t,2) #transpose to plot by scenario

#Plot nMDS coloured by scenario, with legend
# Set up a colour palette - colours applied in the order specified in this list.
library(RColorBrewer)
display.brewer.all()
CE.palette <- brewer.pal(10, "Paired")
display.brewer.pal(10, "Paired")
CE.palette.CPhigh <- brewer.pal(5, "Paired")
CE.names <- colnames(CE.matrix)
CE.labels <- c("S1 equal", "S1 BD-high", "S1 fish-high", "S1 DIN-high", "S1 flood-high",
                "S3 equal", "S3 BD-high", "S3 fish-high", "S3 DIN-high", "S3 flood-high")
CE.names.CPhigh <- colnames(CE3.matrix)
CE.labels.CPhigh <- c("S3 equal", "S3 BD-high", "S3 fish-high", "S3 DIN-high", "S3 flood-high")

#with CP low and high scenarios - can't as have different number of profitable sites
# plot dendogram and ordination of CE site ranks across different scenarios
tiff("CE.dendo.ord.fig.tif", units="mm", width=190, height=95, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,2))
plot(hc.CE.jac.t, labels=CE.labels, xlab = "CE scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("b", line=0, side=3, adj=0, cex=1.5)
plot(CE.t.mds$points, type='n', xlab='NMDS1', ylab='NMDS2')
points(CE.t.mds$points, col=CE.palette, pch=16)
legend("bottomright", CE.labels, 
       fill=CE.palette, ncol=2, #x.intersp=0.5, y.intersp=1,
       cex=0.8, bty="n") #text.width=0, xjust=0.5)
mtext("d", line=0, side=3, adj=0, cex=1.5)
dev.off()

tiff("CE.dendo.fig.tif", units="mm", width=95, height=95, res=800, compression = "lzw")
par(mfrow=c(1,1), mar=c(4,4,1,2))
plot(hc.CE.jac.t, labels=CE.labels, xlab = "EP Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("d", line=0, side=3, adj=0, cex=1.5)
dev.off()

#with CP high scenario
tiff("CE.dendo.ord.fig.CPhigh.tif", units="mm", width=190, height=95, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,2))
plot(hc.CE.jac.t, labels=CE.labels.CPhigh, xlab = "Scenario", ylab="Dissimilarity", cex.lab=1, cex=1, main="")
mtext("c", line=0, side=3, adj=0, cex=1.5)
plot(CE.t.mds$points, type='n', xlab='NMDS1', ylab='NMDS2')
points(CE.t.mds$points, col=CE.palette.CPhigh, pch=19)
legend(x=-0.075,y=-0.02, CE.labels.CPhigh, 
       fill=CE.palette.CPhigh, ncol=1, #x.intersp=0.5, y.intersp=1,
       cex=1, bty="n") #text.width=0, xjust=0.5)
mtext("d", line=0, side=3, adj=0, cex=1.5)
dev.off()

tiff("CE.dendo.fig.CPhigh.tif", units="mm", width=95, height=95, res=800, compression = "lzw")
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(hc.CE.jac.t, labels=CE.labels.CPhigh, xlab = "EP Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("d", line=0, side=3, adj=0, cex=1.5)
dev.off()

# plot - CE score and area
tiff("CE.area.fig.tif", units="mm", width=190, height=120, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(CE_equal~restor_ha, data=CE2, xlab="Area (ha)", ylab="Cost-effectiveness score (S1)", xlim=c(0,5100))
mtext("a", line=0, side=3, adj=0, cex=1.5)
plot(CE_equal_CPhigh~restor_ha, data=CE3, xlab="Area (ha)", ylab="Cost-effectiveness score (S3)", xlim=c(0,5100))
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()

tiff("NPV.area.fig.tif", units="mm", width=190, height=120, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(NPV_25_4_CPlow_FGM2_RClow~restor_ha, data=CE, xlab="Area (ha)", ylab="NPV AU$ 25 years (S1)", xlim=c(0,5100))
mtext("a", line=0, side=3, adj=0, cex=1.5)
plot(NPV_25_4_CPhigh_FGM2_RClow~restor_ha, data=CE, xlab="Area (ha)", ylab="NPV AU$ 25 years (S3)", xlim=c(0,5100))
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()


#--- INVESTIGATE TRADE-OFFS AND SYNERGIES AMONG CARBON AND CO-BENEFITS

colnames(CE) 
dim(CE)
list(c.abate$FID_site) #relabel from 1-43
c.abate$FID_site <- c(1:43)
#carbon abatement per year
CE <- left_join(CE, c.abate, by="FID_site")
CE$abate_CO2eMg_yr <- CE$abate_CO2eMg_25yrs/25
hist(CE$abate_CO2eMg_yr, breaks = 100)
hist(log(CE$abate_CO2eMg_yr), breaks = 100)
CE$log.abate_CO2eMg_yr <- log(CE$abate_CO2eMg_yr) #make some pre-transformed variables
#compare logged carbon abatement per site per year and raw variables
ben.df <- CE %>% dplyr::select(abate_CO2eMg_yr,log.abate_CO2eMg_yr, 
                                BIO_NN_imp_wetland, BIO_NN_RAMSAR, EVNT.n, EPBCcomm, Rsite_area, #biodiversity
                               FISH_hab_for, FISH_conn_to_CAPAD, #fisheries
                               TN_perkm, FEAS_disttosea_km, #DIN removal
                               flood_area) #flood

#pearson correlation coefficient
library("Hmisc")
ben.rcorr = rcorr(as.matrix(ben.df), type = "pearson") #generates one output with several tables
ben.coeff = ben.rcorr$r
write.csv(ben.coeff, file="correlation.matrix.benefits.csv", row.names = T)

#pairs plot
library(psych)
tiff("pairsplot.benefits.tif", units="mm", width=200, height=200, res=800, compression = "lzw")
pairs.panels(ben.df, 
             method = "pearson", # correlation method
             hist.col = "grey",
             density = FALSE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             lm = TRUE,
             labels=c("carbon log", "wetland dist", "ramsar dist", "EVNT", "saltmarsh TEC", "area", 
                      "fish habitat", "fish dist", 
                     "TN conc", "sea dist", "flood zone"),
             cex.cor = 1,
             scale=FALSE,
             gap=0,
             #stars = FALSE, #show the significance of correlations
             main="")
dev.off()


##Analyse the relationships between carbon abatement and cobenefits, using separate linear models for each cobenefit measure and region. 
##Assess relationships with Pearson correlation coefficients. 
##Analyse regions (north Qld, south west WA, north WA) separately because they differ substantially in biodiversity and environmental conditions

tiff("benefits.fig.tif", units="mm", width=200, height=267, res=800, compression = "lzw")
par(mfrow=c(4,3), mar=c(4,4,2,1), mgp=c(2,0.5,0), tck=-0.05)

range(ben.df$BIO_NN_imp_wetland)
#hist(ben.df$BIO_NN_imp_wetland)
#hist(log1p(ben.df$BIO_NN_imp_wetland)) #no transform
m.c.wet <- lm(log.abate_CO2eMg_yr ~ BIO_NN_imp_wetland, data=ben.df)
summary(m.c.wet) 
plot(log.abate_CO2eMg_yr~BIO_NN_imp_wetland, data=ben.df, xlab="Existing wetland distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]" ) 
abline(m.c.wet)
cor(ben.df$log.abate_CO2eMg_yr, ben.df$BIO_NN_imp_wetland, method = 'pearson')
mtext("a", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.19", line=0.5, side=3, adj=1, cex=1) #add pearson
 
range(ben.df$BIO_NN_RAMSAR)
#hist(ben.df$BIO_NN_RAMSAR) 
#hist(log1p(ben.df$BIO_NN_RAMSAR)) #no transform
m.c.ram <- lm(log.abate_CO2eMg_yr ~ BIO_NN_RAMSAR, data=ben.df)
summary(m.c.ram) 
plot(log.abate_CO2eMg_yr~BIO_NN_RAMSAR, data=ben.df, xlab="Ramsar wetland distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.ram)
cor(ben.df$log.abate_CO2eMg_yr, ben.df$BIO_NN_RAMSAR, method = 'pearson')
mtext("b", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.16", line=0.5, side=3, adj=1, cex=1) 

range(ben.df$EVNT.n)
#hist(ben.df$EVNT.n)
#hist(log1p(ben.df$EVNT.n)) #log1p transform
m.c.evnt <- lm(log.abate_CO2eMg_yr ~ log1p(EVNT.n), data=ben.df)
summary(m.c.evnt) 
plot(log.abate_CO2eMg_yr~log1p(EVNT.n), data=ben.df, xlab="Threatened species (abundance) [log]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.evnt)
cor(ben.df$log.abate_CO2eMg_yr, log1p(ben.df$EVNT.n), method = 'pearson')
mtext("c", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.31", line=0.5, side=3, adj=1, cex=1) 

range(ben.df$EPBCcomm)
#hist(ben.df$EPBCcomm)
#hist(log1p(ben.df$EPBCcomm)) #log1p transform
m.c.tec <- lm(log.abate_CO2eMg_yr ~ log1p(EPBCcomm), data=ben.df)
summary(m.c.tec)
plot(log.abate_CO2eMg_yr ~ log1p(EPBCcomm), data=ben.df, xlab="Saltmarsh habitat (ha) [log]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.tec)
cor(ben.df$log.abate_CO2eMg_yr, log1p(ben.df$EPBCcomm), method = 'pearson')
mtext("d", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.35", line=0.5, side=3, adj=1, cex=1) 

range(ben.df$FISH_hab_for)
#hist(ben.df$FISH_hab_for)
#hist(log1p(ben.df$FISH_hab_for)) #log1p transform
m.c.fishhab <- lm(log.abate_CO2eMg_yr ~ log1p(FISH_hab_for), data=ben.df)
summary(m.c.fishhab)
plot(log.abate_CO2eMg_yr~log1p(FISH_hab_for), data=ben.df, xlab="Major watercourse and floodplain (ha) [log]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.fishhab)
cor(ben.df$log.abate_CO2eMg_yr, log1p(ben.df$FISH_hab_for), method = 'pearson')
mtext("e", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.27", line=0.5, side=3, adj=1, cex=1) 

range(ben.df$FISH_conn_to_CAPAD) #NAs
#hist(ben.df$FISH_conn_to_CAPAD)
#hist(log(ben.df$FISH_conn_to_CAPAD)) #could log transform
m.c.fishcon <- lm(log.abate_CO2eMg_yr ~ FISH_conn_to_CAPAD, data=ben.df)
summary(m.c.fishcon) 
plot(log.abate_CO2eMg_yr~FISH_conn_to_CAPAD, data=ben.df, xlab="Marine reserve distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.fishcon)
cor(ben.df$log.abate_CO2eMg_yr, ben.df$FISH_conn_to_CAPAD, method = 'pearson')
mtext("f", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.30", line=0.5, side=3, adj=1, cex=1) 

range(na.omit(ben.df$TN_perkm))
#hist(ben.df$TN_perkm)
#hist(log1p(ben.df$TN_perkm)) #no log transform
m.c.tn <- lm(log.abate_CO2eMg_yr ~ TN_perkm, data=ben.df)
summary(m.c.tn) 
plot(log.abate_CO2eMg_yr~ TN_perkm, data=ben.df, xlab="TN river concentration (kg km\u00B2-1)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.tn)
cor(ben.df$log.abate_CO2eMg_yr, ben.df$TN_perkm, method = 'pearson')
mtext("g", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.13", line=0.5, side=3, adj=1, cex=1) 

range(ben.df$FEAS_disttosea_km)
#hist(ben.df$FEAS_disttosea_km)
#hist(log1p(ben.df$FEAS_disttosea_km)) #could log transform
m.c.seadist <- lm(log.abate_CO2eMg_yr ~ FEAS_disttosea_km, data=ben.df)
summary(m.c.seadist)
plot(log.abate_CO2eMg_yr~FEAS_disttosea_km, data=ben.df, xlab="Distance to sea (km)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.seadist) 
cor(ben.df$log.abate_CO2eMg_yr, ben.df$FEAS_disttosea_km, method = 'pearson')
mtext("h", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.26", line=0.5, side=3, adj=1, cex=1) 

range(ben.df$flood_area) 
#hist(ben.df$flood_area)
#hist(log1p(ben.df$flood_area)) #no log transform
m.c.flood <- lm(log.abate_CO2eMg_yr ~ flood_area, data=ben.df)
summary(m.c.flood)
plot(log.abate_CO2eMg_yr~flood_area, data=ben.df, xlab="1% AEP flood (ha)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.flood) 
cor(ben.df$log.abate_CO2eMg_yr, ben.df$flood_area, method = 'pearson')
mtext("i", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.07", line=0.5, side=3, adj=1, cex=1) 

# but also very correlated with area of the site 
range(ben.df$Rsite_area)
#hist(ben.df$Rsite_area)
#hist(log(ben.df$Rsite_area)) #log transform
m.c.area <- lm(log.abate_CO2eMg_yr ~ log(Rsite_area), data=ben.df)
summary(m.c.area) 
plot(log.abate_CO2eMg_yr~log(Rsite_area), data=ben.df, xlab="Restoration area (ha) [log]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.area)
cor(ben.df$log.abate_CO2eMg_yr, log(ben.df$Rsite_area), method = "pearson")
mtext("j", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.98", line=0.5, side=3, adj=1, cex=1) 

dev.off()


