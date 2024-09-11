# Calculates co-benefits of restoration sites and undertakes the cost-effectiveness analysis
# Valerie Hagger 13/02/2024
rm(list = ls())
setwd("R:/NESP115-Q4418/R_working_directory/Fitzroy") #sets the working directory

# load packages
library(tidyverse)
library(readxl)

# read in restoration sites
CBA <- read.csv("CBA.df.csv", header=T)
colnames(CBA)
dim(CBA)
#select columns needed for cost effectiveness analysis
CE <- CBA %>% dplyr::select(FID_site, Area_ha, BASIN_NAME, abate_CO2eMg_25yrs, NPV_25_4_CPlow_RClow, NPV_25_4_CPlow_RChigh, NPV_25_4_CPhigh_RClow,
                      NPV_25_7_CPlow_RClow, NPV_25_4_CPlow_RClow_upperCI, NPV_25_4_CPlow_RClow_lowerCI)

#---Restoration Feasibility---
#no canals, only barriers within rest sites
rest.sites.bars <- read_excel("rsites4_barriers_join.xls", sheet="rsites4_barriers_join")
head(rest.sites.bars)
range(rest.sites.bars$FID)
range(CE$FID_site)
colnames(rest.sites.bars)[colnames(rest.sites.bars)=="FID"] <- "FID_site"
colnames(rest.sites.bars)[colnames(rest.sites.bars)=="Count_"] <- "bars_n"
rest.sites.bars <- rest.sites.bars[,-c(2:5)]
#assign probability based on number of barriers within site (can't have 0 feasibility) between 0.25-1
rest.sites.bars$bars_prob <- ifelse(rest.sites.bars$bars_n==1,0.5,0.25)
rest.sites.bars$bars_prob <- ifelse(rest.sites.bars$bars_n>1,0.75,rest.sites.bars$bars_prob)
rest.sites.bars$bars_prob <- ifelse(rest.sites.bars$bars_n>=10,1,rest.sites.bars$bars_prob)
table(rest.sites.bars$bars_n) #412 with none, 25 with 1, some have more than 1
hist(rest.sites.bars$bars_n, breaks = 100)
table(rest.sites.bars$bars_prob)

rest.sites.tidal <- read_excel("rsites4_tidal.xls", sheet="rsites4_tidal")
head(rest.sites.tidal)
rest.tidal.tidy <- rest.sites.tidal %>% 
  dplyr::select(FID_site="FID_tidal_", Area_ha, Tidal_zone="gridcode", "Tidal_ha") %>% 
  spread(key = Tidal_zone, value = "Tidal_ha", convert=TRUE)
head(rest.tidal.tidy) #1 is low, 2 is mid, 3 is high
colnames(rest.tidal.tidy)[3] <- "low_tidal_ha" 
colnames(rest.tidal.tidy)[4] <- "mid_tidal_ha" 
colnames(rest.tidal.tidy)[5] <- "high_tidal_ha" 
rest.tidal.tidy[is.na(rest.tidal.tidy)] <- 0

rest.tidal.tidy$low_tidal_prop <- rest.tidal.tidy$low_tidal_ha  / rest.tidal.tidy$Area_ha

#investigate range of low tidal zone
range(rest.tidal.tidy$low_tidal_ha) #122 ha max
hist(rest.tidal.tidy$low_tidal_ha, breaks = 200) #most sites have minimal amount
quantile(rest.tidal.tidy$low_tidal_ha) #median is 0
range(rest.tidal.tidy$low_tidal_prop) #1 highest
hist(rest.tidal.tidy$low_tidal_prop, breaks=100) 
quantile(rest.tidal.tidy$low_tidal_prop)#median is 0
quantile(rest.tidal.tidy$low_tidal_ha, prob = seq(0, 1, length = 11), type = 5) #take max as 90%

# assign probability based on proportion of site within low intertidal zone - between 0.25 and 1
rest.tidal.tidy$low_tidal_prob <- ifelse(rest.tidal.tidy$low_tidal_prop>=0.1,0.5,0.25)
rest.tidal.tidy$low_tidal_prob <- ifelse(rest.tidal.tidy$low_tidal_prop>=0.2,0.75,rest.tidal.tidy$low_tidal_prob) 
rest.tidal.tidy$low_tidal_prob <- ifelse(rest.tidal.tidy$low_tidal_prop>=0.4,1,rest.tidal.tidy$low_tidal_prob) 
table(rest.tidal.tidy$low_tidal_prob) #386 sites with no low, 60 with >0-25%, 5 with 25-50%, 1 with 50-75% and 3 with 75-100%

# combine and take average
rest.feas <- full_join(rest.tidal.tidy, rest.sites.bars, by = "FID_site")
colnames(rest.feas)
rest.feas$feas <- (rest.feas$low_tidal_prob + rest.feas$bars_prob)/2 #between 0.25 (low) and 1 (high), none with both 1
table(rest.feas$feas) # better spread
rest.feas <- rest.feas %>% dplyr::select(FID_site, low_tidal_prob, bars_prob, feas)


#---Estimate co-benefits--- 
# Biodiversity benefit
## wetland connectivity
rest.sites.wetland <- read_excel("rsites4_wetland_nomod.xls", sheet="rsites4_wetland_nomod")
colnames(rest.sites.wetland)
colnames(rest.sites.wetland) <- c("Object_ID", "FID_site", "Near_wetland_FID", "Near_wetland_m")
min(rest.sites.wetland$Near_wetland_m) # max benefit is adjacent (0) - give 100% to 0
max(rest.sites.wetland$Near_wetland_m) # min benefit is 4.6km away - give 0% to max distance
rest.sites.wetland$wetland_dist_scale <- 100 - ((rest.sites.wetland$Near_wetland_m/max(rest.sites.wetland$Near_wetland_m))*100) #work out proportion
head(rest.sites.wetland)
rest.sites.wetland <- rest.sites.wetland[,-c(1,3)]
range(rest.sites.wetland$wetland_dist_scale) #check
#higher prop, closer distance

## Ramsar connectivity
rest.sites.ramsar <- read_excel("rsites4_ramsar.xls", sheet="rsites4_ramsar")
head(rest.sites.ramsar)
rest.sites.ramsar <- rest.sites.ramsar[,-c(1,3)]
colnames(rest.sites.ramsar) <- c("FID_site", "Near_ramsar_m")
min(rest.sites.ramsar$Near_ramsar_m) # max benefit is adjacent (0)
max(rest.sites.ramsar$Near_ramsar_m) # min benefit is 125km away
#scale between 0 (lowest benefit) and 100% (highest benefit) - utility function requires %
rest.sites.ramsar$ramsar_dist_scale <- 100 - ((rest.sites.ramsar$Near_ramsar_m/max(rest.sites.ramsar$Near_ramsar_m))*100) #work out proportion
head(rest.sites.ramsar)
range(rest.sites.ramsar$ramsar_dist_scale) #check

## EVNT taxa - number of records within 1000m of site
rest.sites.EVNT <- read_excel("rsites4_EVNT.xls", sheet="rsites4_EVNT")
head(rest.sites.EVNT) #IN_FID is FID_site, NEAR_FID is each EVNT record
rest.sites.EVNT.n <- with(rest.sites.EVNT, tapply(NEAR_FID, IN_FID, length)) %>% as.data.frame
colnames(rest.sites.EVNT.n) <- "EVNT_n" 
rest.sites.EVNT.n <- rest.sites.EVNT.n %>% tibble::rownames_to_column("FID_site") 
rest.sites.EVNT.n$EVNT_n_scale <- (rest.sites.EVNT.n$EVNT_n/max(rest.sites.EVNT.n$EVNT_n))*100
head(rest.sites.EVNT.n)
range(rest.sites.EVNT.n$EVNT_n) #NAs will be given to missing sites when joining dataframes
rest.sites.EVNT.n$FID_site <- as.numeric(rest.sites.EVNT.n$FID_site)
str(rest.sites.EVNT.n)
hist(rest.sites.EVNT.n$EVNT_n)
## Patch size
#Use Area_ha of each site

## Capricorn Yellow Chat (CYC) - habitat within population areas
rest.sites.CYChab <- read_excel("rsites4_CYChab_convex.xls", sheet="rsites4_CYChab_convex")
head(rest.sites.CYChab)
dim(rest.sites.CYChab) #not all sites have habitat, NAS will be given to missing sites when joining dataframes
range(rest.sites.CYChab$FID_tidal_)
rest.sites.CYChab <- rest.sites.CYChab %>%
  dplyr::select(FID_site="FID_tidal_", "Area_ha", "CYChab_ha")
range(rest.sites.CYChab$CYChab_ha)
rest.sites.CYChab$CYC_hab_scale <- (rest.sites.CYChab$CYChab_ha/max(rest.sites.CYChab$CYChab_ha))*100
rest.sites.CYChab$Patch_size_scale <- (rest.sites.CYChab$Area_ha/max(rest.sites.CYChab$Area_ha))*100
rest.sites.CYChab <- rest.sites.CYChab[,-2]

## No go areas - remove sites containing known CYC populations (CYC_sites_buffer) before CE analysis/ ranking
rest.sites.CYCsites <- read_excel("rsites4_CYCsites_1kmbuf.xls", sheet="rsites4_CYCsites_1kmbuf")
colnames(rest.sites.CYCsites)
# Remove if Count_==1
head(rest.sites.CYCsites) #FID = FID_site
rest.sites.CYCsites <- rest.sites.CYCsites %>%
  dplyr::select(FID_site="FID", CYC_site="Count_")


# Coastal fisheries habitat
## 3 indicators - distance to permanent watercourse, distance to fish habitat, and area of mangrove habitat/distance to seagrass
## watercourse connectivity
rest.sites.3order <- read_excel("rsites4_3order.xls", sheet="rsites4_3order")
head(rest.sites.3order)
rest.sites.3order <- rest.sites.3order[,-c(1,3)]
colnames(rest.sites.3order) <- c("FID_site", "Near_3order_m")
list(rest.sites.3order$FID_site)
min(rest.sites.3order$Near_3order_m) # max benefit is adjacent (0)
max(rest.sites.3order$Near_3order_m) #min benefit is 10km away
rest.sites.3order$dist_3order_scale <- 100 - ((rest.sites.3order$Near_3order_m/max(rest.sites.3order$Near_3order_m))*100) #or max could be adjacent
range(rest.sites.3order$dist_3order_scale) #check
#higher prop, closer distance to stream

## fish habitat areas
rest.sites.FHA <- read_excel("rsites4_FHA.xls", sheet="rsites4_FHA")
head(rest.sites.FHA)
rest.sites.FHA <- rest.sites.FHA[,-c(1,3)]
colnames(rest.sites.FHA) <- c("FID_site", "Near_FHA_m")
min(rest.sites.FHA$Near_FHA_m) # max benefit is adjacent (0)
max(rest.sites.FHA$Near_FHA_m) #min benefit is 33km away
rest.sites.FHA$dist_FHA_scale <- 100 - ((rest.sites.FHA$Near_FHA_m/max(rest.sites.FHA$Near_FHA_m))*100) #or max could be adjacent
range(rest.sites.FHA$dist_FHA_scale) #check
#higher percent or prop, closer distance to fish habitat area

## Area in the lower intertidal zone
head(rest.tidal.tidy)
rest.tidal.tidy$low_tidal_scale <- (rest.tidal.tidy$low_tidal_ha/max(rest.tidal.tidy$low_tidal_ha))*100
rest.low.tidal <- rest.tidal.tidy[,-c(2,4:7)]
head(rest.low.tidal)

# DIN removal - Denitrification
## 3-4 indicators - DIN loads and TSS loads in catchments, retention time (area in lower intertidal zone and area in watercourse >3order buffer)
## DIN loads of sites
basin.DIN.loads <- readxl::read_excel("Catchment_loads.xlsx", sheet="DIN_Waters_2014")
table(basin.DIN.loads$Catchment)
table(CE$BASIN_NAME)
CE$DIN_load <- basin.DIN.loads$DIN_2013_mg_L[match(unlist(CE$BASIN_NAME), basin.DIN.loads$Catchment)] #used concentration not load
table(CE$DIN_load)
range(na.omit(CE$DIN_load)) #NAs for Coral See and Curtis Coast - give those catchments zero
CE$DIN_load[is.na(CE$DIN_load)] <- 0
CE$DIN_load_scale <- (CE$DIN_load/max(CE$DIN_load))*100 #based on max load, 1 means highest load
range(CE$DIN_load_scale)

#TSS
basin.TSS.loads <- read_excel("Catchment_loads.xlsx", sheet="Sediment")
table(basin.TSS.loads$Catchment)
colnames(basin.TSS.loads)
table(CE$BASIN_NAME)
CE$TSS_load <- basin.TSS.loads$Total_fine_sediment_load_2016_2018_kt_ML[match(unlist(CE$BASIN_NAME), basin.TSS.loads$Catchment)]
table(CE$TSS_load)
range(na.omit(CE$TSS_load)) #NAs for Coral See and Curtis Coast - give those catchments zero
CE$TSS_load[is.na(CE$TSS_load)] <- 0
CE$TSS_load_scale <- 100 - ((CE$TSS_load/max(CE$TSS_load))*100) #high loads reduce benefit, because reduce denitrification
range(CE$TSS_load_scale)

## Area in the lower intertidal zone
#aleady done for fisheries

## Area in >3order stream 100m buffer
rest.sites.3order.buf <- read_excel("rsites4_3order_buf_100m.xls", sheet="rsites4_3order_buf2")
colnames(rest.sites.3order.buf)
rest.sites.3order.buf <- rest.sites.3order.buf[,-1]
colnames(rest.sites.3order.buf)<- c("FID_site", "intersect_3order_ha")
range(rest.sites.3order.buf$intersect_3order_ha)
dim(rest.sites.3order.buf) #not all sites intersect with stream buffer, NAs will be given to missing sites when joining dataframes
rest.sites.3order.buf$intersect_3order_scale <- (rest.sites.3order.buf$intersect_3order_ha / max(rest.sites.3order.buf$intersect_3order_ha))*100


# Flood mitigation
## 2 indicators: Inland flooding (area within flood mapping extent - decreases erosion during inland flood events) and 
##Coastal flooding (area with mangroves - decreases wave/wind energy during storm surges and accretes sediment to prevent coastal flooding)
## 1) Flood extent area
rest.sites.flood <- read_excel("rsites4_Fitzroy_flood.xls", sheet="rsites4_Fitzroy_flood")
colnames(rest.sites.flood)
rest.sites.flood.tidy <- rest.sites.flood %>% 
  dplyr::select(FID_site="FID_tidal_", Flood_type="type", "flood_ha") %>% 
  spread(key = Flood_type, value = "flood_ha", convert=TRUE)
colnames(rest.sites.flood.tidy) #take 100yr peak
rest.sites.flood.tidy <- rest.sites.flood.tidy[,-3]
dim(rest.sites.flood.tidy) #not all sites intersect with flood zone, NAs give to missing sites when joining dataframes - fix later
rest.sites.flood.tidy$Fitzroy_100yr_scale <- (rest.sites.flood.tidy$Fitzroy_100m_100yr_peak / max(rest.sites.flood.tidy$Fitzroy_100m_100yr_peak))*100

## 2) Mangrove area 
rest.sites.BVG <- read_excel("rsites4_preclearRE.xls", sheet="rsites4_preclearRE")
head(rest.sites.BVG)
# select and spread data from long to wide
rest.sites.BVG.tidy <- rest.sites.BVG %>%
  dplyr::select(FID_site="FID_tidal_", "DBVG1M", "BVG_ha") %>% 
  spread(key = "DBVG1M", value = "BVG_ha", convert=TRUE) # make sure there are no duplicate sites, if so need to dissolve in ArcGIS first
head(rest.sites.BVG.tidy)
#just take mangrove area
rest.sites.mang <- rest.sites.BVG.tidy %>% dplyr::select("FID_site", mang_ha="35a")
rest.sites.mang$mang_ha[is.na(rest.sites.mang$mang_ha)] <- 0
rest.sites.mang$mang_ha_scale <- (rest.sites.mang$mang_ha/max(rest.sites.mang$mang_ha))*100
head(rest.sites.mang)

# Cultural heritage
## do not include in co-benefits, but identify sites with potential TO-led restoration opportunity
## 4 indicators - native title, registered parties, protected sites and leasehold, state and commonwealth land
## 1) native title
rsites.nativetitle <- read_excel("rsites4_nativetitle.xls", sheet="rsites4_nativetitle")
colnames(rsites.nativetitle)
rsites.nativetitle.tidy <- rsites.nativetitle %>% 
  dplyr::select(FID_site="FID_tidal_", Area_ha, title="Determin_2", "nat_tit_ha") %>% 
  spread(key = title, value = "nat_tit_ha", convert=TRUE)
head(rsites.nativetitle.tidy)
colnames(rsites.nativetitle.tidy) <- c("FID_site", "Area_ha", "non_exclusive_ha")
rsites.nativetitle.tidy$non_exclusive_prop <- rsites.nativetitle.tidy$non_exclusive_ha/rsites.nativetitle.tidy$Area_ha
rsites.nativetitle.tidy <- rsites.nativetitle.tidy %>% dplyr::select(-Area_ha)

## 2) registered parties
rsites.parties <- read_excel("rsites4_CHparty.xls", sheet="rsites4_CHparty")
colnames(rsites.parties)
rsites.parties.tidy <- rsites.parties %>% 
  dplyr::select(FID_site="FID_tidal_", "Area_ha", party="name", "party_ha") %>% 
  spread(key = party, value = "party_ha", convert=TRUE)
table(rsites.parties.tidy$FID_site)
dim(rsites.parties.tidy) #all sites covered
rsites.parties.tidy$parties_ha <- rowSums(rsites.parties.tidy[3:6], na.rm=TRUE)
rsites.parties.tidy$parties_prop <- rsites.parties.tidy$parties_ha/rsites.parties.tidy$Area_ha
range(rsites.parties.tidy$parties_ha)
range(rsites.parties.tidy$parties_prop)
rsites.parties.tidy[49:50,] #FID site 52 and 53 have low registered parties, but otherwise all sites do
rsites.parties.tidy <- rsites.parties.tidy %>% dplyr::select(-Area_ha)
colnames(rsites.parties.tidy)
range(rsites.parties.tidy$parties_prop) #all sites have a registered Aboriginal party
table(rsites.parties$name) #all sites have a registered Aboriginal party and only 1 per site
dim(rsites.parties)
dim(rsites.parties.tidy)
rsites.parties.name <- rsites.parties %>% dplyr::select(FID_site="FID_tidal_", name)
rsites.parties.tidy <- full_join(rsites.parties.tidy, rsites.parties.name, by="FID_site")

## 3) protected sites 
# no protected sites in Fitzroy case study region

## 4) leasehold, state and commonwealth land
rsites.tenure <- read_excel("rsites4_tenure.xls", sheet="rsites4_tenure")
colnames(rsites.tenure)
rsites.tenure.tidy <- rsites.tenure %>% 
  dplyr::select(FID_site="FID_tidal_", "Area_ha", Tenure="TENURE", "Tenure_ha") %>% 
  spread(key = Tenure, value = "Tenure_ha", convert=TRUE)
head(rsites.tenure.tidy) #only reserve and state land
table(rsites.tenure.tidy$FID_site) #check no duplications
dim(rsites.tenure.tidy) #only 47 sites with potential tenure for native title
rsites.tenure.tidy$tenure_ha <- rowSums(rsites.tenure.tidy[3:4], na.rm=TRUE)
rsites.tenure.tidy$tenure_prop <- rsites.tenure.tidy$tenure_ha/rsites.tenure.tidy$Area_ha
rsites.tenure.tidy <- rsites.tenure.tidy %>% dplyr::select(-Area_ha)
colnames(rsites.tenure.tidy)

## join feasibility and co-benefits into CE dataframe
colnames(CE)
#feasibility
CE <- full_join(CE, rest.feas, by="FID_site")
#DIN removal
CE <- full_join(CE, rest.low.tidal, by="FID_site") #DIN and TSS already added
CE <- full_join(CE, rest.sites.3order.buf, by="FID_site")
#biodiversity
CE <- full_join(CE, rest.sites.wetland, by="FID_site")
CE <- full_join(CE, rest.sites.ramsar, by="FID_site")
CE <- full_join(CE, rest.sites.EVNT.n, by="FID_site")
CE <- full_join(CE, rest.sites.CYChab, by="FID_site")
CE <- full_join(CE, rest.sites.CYCsites, by="FID_site")
#fisheries
CE <- full_join(CE, rest.sites.3order, by="FID_site")
CE <- full_join(CE, rest.sites.FHA, by="FID_site") #low tidal already added
#flood mitigation
CE <- full_join(CE, rest.sites.flood.tidy, by="FID_site") 
CE <- full_join(CE, rest.sites.mang, by="FID_site")
#cultural heritage 
CE <- full_join(CE, rsites.nativetitle.tidy, by="FID_site")
CE <- full_join(CE, rsites.parties.tidy, by="FID_site")
CE <- full_join(CE, rsites.tenure.tidy, by="FID_site")
colnames(CE)
#fix NAs given to missing sites
range(CE$EVNT_n_scale)
CE$EVNT_n_scale[is.na(CE$EVNT_n_scale)] <- 0
CE$CYC_hab_scale[is.na(CE$CYC_hab_scale)] <- 0
CE$intersect_3order_scale[is.na(CE$intersect_3order_scale)] <- 0
CE$Fitzroy_100yr_scale[is.na(CE$Fitzroy_100yr_scale)] <- 0

range(CE$EVNT_n)
CE$EVNT_n[is.na(CE$EVNT_n)] <- 0
CE$CYChab_ha[is.na(CE$CYChab_ha)] <- 0
CE$intersect_3order_ha[is.na(CE$intersect_3order_ha)] <- 0
CE$Fitzroy_100m_100yr_peak[is.na(CE$Fitzroy_100m_100yr_peak)] <- 0

which.max(CE$Area_ha) #FID328 is biggest site


#---S7 Cost effectiveness analysis ---

# Calculate cobenefits (B)
# B = sum (Bn*Wn)
# total indicators: biodiversity (5), fisheries (3), DIN (4), flood (2) = 14, however 13 in total because mangrove area used twice 

# first do some pairs plots do look for collinearity/trade-offs between functions
indicators.raw <- CE %>% dplyr::select(Near_wetland_m, Near_ramsar_m, EVNT_n, CYChab_ha, #biodiversity
                            Near_3order_m, Near_FHA_m, low_tidal_ha, #fisheries
                            DIN_load, TSS_load, intersect_3order_ha, #DIN removal
                            Fitzroy_100m_100yr_peak, mang_ha, Area_ha) #flood plus mangrove area pc
indicators.scaled <- CE %>% dplyr::select(wetland_dist_scale, ramsar_dist_scale, EVNT_n_scale, CYC_hab_scale, Patch_size_scale, #biodiversity
                                dist_3order_scale, dist_FHA_scale, mang_ha_scale, #fisheries
                                DIN_load_scale, TSS_load_scale, low_tidal_scale, intersect_3order_scale, #DIN removal
                                Fitzroy_100yr_scale) #flood plus mangrove area pc
#John's function for pairs plots
panel.regression <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                              cex = 1, col.regres = "red", ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    mod<-stats::lm(y[ok] ~ x[ok])
  mod2<-stats::lm(y[ok] ~ x[ok] + I(x[ok]^2))
  if(AICc(mod)<AICc(mod2))
    curve(cbind(1,x)%*%coef(mod),add=T, col = col.regres, ...) else
      curve(cbind(1,x,x^2)%*%coef(mod2),add=T, col = col.regres, ...)
} 
library(MuMIn)

pairs(indicators.raw, panel=panel.regression)
pairs(indicators.scaled, panel=panel.regression)
library("Hmisc")
raw.rcorr = rcorr(as.matrix(indicators.raw), type = "pearson") #generates one output with several tables
raw.coeff = raw.rcorr$r

# Correlations
#CYC habitat and patch size
#CYC habitat and area intersected by <3order stream
#Patch size and area intersected by <3order stream
#Patch size and area within 100 year flood event
#DIN and TSS loads
#Area intersected by 3order stream and within 100 year flood event

# remove patch size from biodiversity as correlated with a few others (as well as financial benefit)

# difficult for stakeholders to assign weightings because they are all biased in particular areas (i.e. biodiversity, cultural heritage, carbon)
# tallied the number that each value was mentioned by a stakeholder: Biodiversity (4), water quality (3), Cultural heritage (3), fisheries (2), carbon (1), storm protection (1), sustainable grazing (1), community resilience (1)           
# equal weighting of ecosystem services - BD, fish, water, flood each 0.25 (should sum to 1)
# then divide indicator weightings among service weightings - BD (0.25/4), fish (0.25/3), DIN (0.25/4), flood (0.25/2)
CE$ES_equal <- (CE$wetland_dist_scale*(0.25/4)) + (CE$ramsar_dist_scale*(0.25/4)) + (CE$EVNT_n_scale*(0.25/4)) + (CE$CYC_hab_scale*(0.25/4)) + #biodiversity 
  (CE$dist_3order_scale*(0.25/3)) + (CE$dist_FHA_scale*(0.25/3)) + (CE$low_tidal_scale*(0.25/3)) + #fisheries
  (CE$DIN_load_scale*(0.25/4)) + (CE$TSS_load_scale*(0.25/4)) + (CE$low_tidal_scale*(0.25/4)) + (CE$intersect_3order_scale*(0.25/4)) + #DIN removal
  (CE$Fitzroy_100yr_scale*(0.25/2)) + (CE$mang_ha_scale*(0.25/2)) #flood

# stakeholder weightings of ecosystem services - BD (4/10=0.4) fish (2/10=0.2), DIN (3/10=0.3), flood (1/10=0.1)
CE$ES_stake <- (CE$wetland_dist_scale*(0.4/4)) + (CE$ramsar_dist_scale*(0.4/4)) + (CE$EVNT_n_scale*(0.4/4)) + (CE$CYC_hab_scale*(0.4/4)) + #biodiversity 
  (CE$dist_3order_scale*(0.2/3)) + (CE$dist_FHA_scale*(0.2/3)) + (CE$low_tidal_scale*(0.2/3)) + #fisheries
  (CE$DIN_load_scale*(0.3/4)) + (CE$TSS_load_scale*(0.3/4)) + (CE$low_tidal_scale*(0.3/4)) + (CE$intersect_3order_scale*(0.3/4)) + #DIN removal
  (CE$Fitzroy_100yr_scale*(0.1/2)) + (CE$mang_ha_scale*(0.1/2)) #flood

# each ES high (0.7) with 0.1 each to others = 1
CE$ES_BD <- (CE$wetland_dist_scale*(0.7/4)) + (CE$ramsar_dist_scale*(0.7/4)) + (CE$EVNT_n_scale*(0.7/4)) + (CE$CYC_hab_scale*(0.7/4)) + #biodiversity 
  (CE$dist_3order_scale*(0.1/3)) + (CE$dist_FHA_scale*(0.1/3)) + (CE$low_tidal_scale*(0.1/3)) + #fisheries
  (CE$DIN_load_scale*(0.1/4)) + (CE$TSS_load_scale*(0.1/4)) + (CE$low_tidal_scale*(0.1/4)) + (CE$intersect_3order_scale*(0.1/4)) + #DIN removal
  (CE$Fitzroy_100yr_scale*(0.1/2)) + (CE$mang_ha_scale*(0.1/2)) #flood

CE$ES_fish <- (CE$wetland_dist_scale*(0.1/4)) + (CE$ramsar_dist_scale*(0.1/4)) + (CE$EVNT_n_scale*(0.1/4)) + (CE$CYC_hab_scale*(0.1/4)) + #biodiversity 
  (CE$dist_3order_scale*(0.7/3)) + (CE$dist_FHA_scale*(0.7/3)) + (CE$low_tidal_scale*(0.7/3)) + #fisheries
  (CE$DIN_load_scale*(0.1/4)) + (CE$TSS_load_scale*(0.1/4)) + (CE$low_tidal_scale*(0.1/4)) + (CE$intersect_3order_scale*(0.1/4)) + #DIN removal
  (CE$Fitzroy_100yr_scale*(0.1/2)) + (CE$mang_ha_scale*(0.1/2)) #flood

CE$ES_DIN <- (CE$wetland_dist_scale*(0.1/4)) + (CE$ramsar_dist_scale*(0.1/4)) + (CE$EVNT_n_scale*(0.1/4)) + (CE$CYC_hab_scale*(0.1/4)) + #biodiversity 
  (CE$dist_3order_scale*(0.1/3)) + (CE$dist_FHA_scale*(0.1/3)) + (CE$low_tidal_scale*(0.1/3)) + #fisheries
  (CE$DIN_load_scale*(0.7/4)) + (CE$TSS_load_scale*(0.7/4)) + (CE$low_tidal_scale*(0.7/4)) + (CE$intersect_3order_scale*(0.7/4)) + #DIN removal
  (CE$Fitzroy_100yr_scale*(0.1/2)) + (CE$mang_ha_scale*(0.1/2)) #flood

CE$ES_flood <- (CE$wetland_dist_scale*(0.1/4)) + (CE$ramsar_dist_scale*(0.1/4)) + (CE$EVNT_n_scale*(0.1/4)) + (CE$CYC_hab_scale*(0.1/4)) + #biodiversity 
  (CE$dist_3order_scale*(0.1/3)) + (CE$dist_FHA_scale*(0.1/3)) + (CE$low_tidal_scale*(0.1/3)) + #fisheries
  (CE$DIN_load_scale*(0.1/4)) + (CE$TSS_load_scale*(0.1/4)) + (CE$low_tidal_scale*(0.1/4)) + (CE$intersect_3order_scale*(0.1/4)) + #DIN removal
  (CE$Fitzroy_100yr_scale*(0.7/2)) + (CE$mang_ha_scale*(0.7/2)) #flood

list(CE$ES_equal) #check for NAs
range(CE$wetland_dist_scale)
range(CE$ramsar_dist_scale)
range(CE$EVNT_n_scale)
#list(CE$EVNT_n_scale) #NAs
range(CE$CYC_hab_scale)
#list(CE$CYC_hab_scale) #NAs
range(CE$dist_3order_scale)
range(CE$dist_FHA_scale)
range(CE$mang_ha_scale)
range(CE$DIN_load_scale)
range(CE$TSS_load_scale)
range(CE$low_tidal_scale)
range(CE$intersect_3order_scale)
#list(CE$intersect_3order_scale) #NAs
range(CE$Fitzroy_100yr_scale)
#list(CE$Fitzroy_100yr_scale) #NAs
range(CE$ES_equal)
range(CE$ES_BD)
range(CE$ES_fish)
range(CE$ES_DIN)
range(CE$ES_flood)
range(CE$feas)

#calculate benefits 
bd <- (CE$wetland_dist_scale*(1/4)) + (CE$ramsar_dist_scale*(1/4)) + (CE$EVNT_n_scale*(1/4)) + (CE$CYC_hab_scale*(1/4)) #biodiversity 
fish <- (CE$dist_3order_scale*(1/3)) + (CE$dist_FHA_scale*(1/3)) + (CE$low_tidal_scale*(1/3)) #fisheries
DIN <-  (CE$DIN_load_scale*(1/4)) + (CE$TSS_load_scale*(1/4)) + (CE$low_tidal_scale*(1/4)) + (CE$intersect_3order_scale*(1/4)) #DIN removal
DIN2 <-  (CE$DIN_load_scale*(1/2)) + (CE$TSS_load_scale*(1/2)) #DIN removal
DIN3 <-  (CE$low_tidal_scale*(1/2)) + (CE$intersect_3order_scale*(1/2)) #DIN removal
flood <-  (CE$Fitzroy_100yr_scale*(1/2)) + (CE$mang_ha_scale*(1/2)) #flood
hist(bd)
hist(fish)
hist(DIN)
hist(DIN2)
hist(DIN3)
hist(flood)

# calculate cost-effectiveness considering NPV, restoration feasibility (F) and cobenefits/ecosystem services (ES)
# remove CYC no go areas
## No go areas - sites containing known CYC populations (CYC_sites_buffer) 
# Remove if CYC_site or Count_==1
CE2 <- subset(CE, CE$CYC_site==0)
dim(CE2)

# relative indicator as to which site is the most effective
# loss: least cost per unit of outcome 
# gain: most profit AND unit of outcome
table(CE2$NPV_25_4_CPhigh_RClow>0) # both positive and negative, so need to subset
## create function
#most effective per $ spent
CE.fun.loss <- function(NPV, ES, F) {
  NPV / (ES * F)
}
CE.fun.profit <- function(NPV, ES, F) {
  NPV * (ES * F)
}

# CE of 2 scenarios - base and high carbon price
# NPV_25_4_CPlow_RClow
##equal weighting
CE.equal <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_RClow, ES_equal, feas) 
CE.equal.prof <- subset(CE.equal, CE.equal$NPV_25_4_CPlow_RClow>0)
CE.equal.loss <- subset(CE.equal, CE.equal$NPV_25_4_CPlow_RClow<=0)
CE.equal.prof$CE_equal <- CE.fun.profit(NPV=CE.equal.prof$NPV_25_4_CPlow_RClow, ES=CE.equal.prof$ES_equal, F=CE.equal.prof$feas)
CE.equal.loss$CE_equal <- CE.fun.loss(NPV=CE.equal.loss$NPV_25_4_CPlow_RClow, ES=CE.equal.loss$ES_equal, F=CE.equal.loss$feas)
CE.equal.prof1 <- CE.equal.prof[,-c(2:4)]
CE.equal.loss1 <- CE.equal.loss[,-c(2:4)]
#CE.equal2 <- bind_rows(CE.equal.loss1, CE.equal.prof1) %>% as.data.frame
CE3 <- right_join(CE2, CE.equal.prof1, by="FID_site")%>% as.data.frame #only rank profit sites
dim(CE3)
CE3$CE_equal_rank <- rank(-CE3$CE_equal)#rank from 1 (highest score) to lowest score (194)
dim(CE3) #check number of sites, less prof sites

##biodiversity high weighting
CE.BD <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_RClow, ES_BD, feas) 
CE.BD.prof <- subset(CE.BD, CE.BD$NPV_25_4_CPlow_RClow>0)
CE.BD.loss <- subset(CE.BD, CE.BD$NPV_25_4_CPlow_RClow<=0)
CE.BD.prof$CE_BD <- CE.fun.profit(NPV=CE.BD.prof$NPV_25_4_CPlow_RClow, ES=CE.BD.prof$ES_BD, F=CE.BD.prof$feas)
CE.BD.loss$CE_BD <- CE.fun.loss(NPV=CE.BD.loss$NPV_25_4_CPlow_RClow, ES=CE.BD.loss$ES_BD, F=CE.BD.loss$feas)
CE.BD.prof1 <- CE.BD.prof[,-c(2:4)]
CE.BD.loss1 <- CE.BD.loss[,-c(2:4)]
#CE.BD2 <- bind_rows(CE.BD.loss1, CE.BD.prof1) %>% as.data.frame
CE3 <- right_join(CE3, CE.BD.prof1, by="FID_site")%>% as.data.frame
CE3$CE_BD_rank <- rank(-CE3$CE_BD)
dim(CE3)

##fisheries high weighting
CE.fish <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_RClow, ES_fish, feas) 
CE.fish.prof <- subset(CE.fish, CE.fish$NPV_25_4_CPlow_RClow>0)
CE.fish.loss <- subset(CE.fish, CE.fish$NPV_25_4_CPlow_RClow<=0)
CE.fish.prof$CE_fish <- CE.fun.profit(NPV=CE.fish.prof$NPV_25_4_CPlow_RClow, ES=CE.fish.prof$ES_fish, F=CE.fish.prof$feas)
CE.fish.loss$CE_fish <- CE.fun.loss(NPV=CE.fish.loss$NPV_25_4_CPlow_RClow, ES=CE.fish.loss$ES_fish, F=CE.fish.loss$feas)
CE.fish.prof1 <- CE.fish.prof[,-c(2:4)]
CE.fish.loss1 <- CE.fish.loss[,-c(2:4)]
#CE.fish2 <- bind_rows(CE.fish.loss1, CE.fish.prof1) %>% as.data.frame
CE3 <- right_join(CE3, CE.fish.prof1, by="FID_site")%>% as.data.frame
CE3$CE_fish_rank <- rank(-CE3$CE_fish)
dim(CE3)

##DIN removal high weighting
CE.DIN <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_RClow, ES_DIN, feas) 
CE.DIN.prof <- subset(CE.DIN, CE.DIN$NPV_25_4_CPlow_RClow>0)
CE.DIN.loss <- subset(CE.DIN, CE.DIN$NPV_25_4_CPlow_RClow<=0)
CE.DIN.prof$CE_DIN <- CE.fun.profit(NPV=CE.DIN.prof$NPV_25_4_CPlow_RClow, ES=CE.DIN.prof$ES_DIN, F=CE.DIN.prof$feas)
CE.DIN.loss$CE_DIN <- CE.fun.loss(NPV=CE.DIN.loss$NPV_25_4_CPlow_RClow, ES=CE.DIN.loss$ES_DIN, F=CE.DIN.loss$feas)
CE.DIN.prof1 <- CE.DIN.prof[,-c(2:4)]
CE.DIN.loss1 <- CE.DIN.loss[,-c(2:4)]
#CE.DIN2 <- bind_rows(CE.DIN.loss1, CE.DIN.prof1) %>% as.data.frame
CE3 <- right_join(CE3, CE.DIN.prof1, by="FID_site")%>% as.data.frame
CE3$CE_DIN_rank <- rank(-CE3$CE_DIN)
dim(CE3)

##flood mitigation high weighting
CE.flood <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPlow_RClow, ES_flood, feas) 
CE.flood.prof <- subset(CE.flood, CE.flood$NPV_25_4_CPlow_RClow>0)
CE.flood.loss <- subset(CE.flood, CE.flood$NPV_25_4_CPlow_RClow<=0)
CE.flood.prof$CE_flood <- CE.fun.profit(NPV=CE.flood.prof$NPV_25_4_CPlow_RClow, ES=CE.flood.prof$ES_flood, F=CE.flood.prof$feas)
CE.flood.loss$CE_flood <- CE.fun.loss(NPV=CE.flood.loss$NPV_25_4_CPlow_RClow, ES=CE.flood.loss$ES_flood, F=CE.flood.loss$feas)
CE.flood.prof1 <- CE.flood.prof[,-c(2:4)]
CE.flood.loss1 <- CE.flood.loss[,-c(2:4)]
#CE.flood2 <- bind_rows(CE.flood.loss1, CE.flood.prof1) %>% as.data.frame
CE3 <- right_join(CE3, CE.flood.prof1, by="FID_site")%>% as.data.frame
CE3$CE_flood_rank <- rank(-CE3$CE_flood)
dim(CE3)

# CE using NPV_25_4_CPhigh_RClow (scenario 4 - higher carbon price)
##equal weighting
CE.equal.CP <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_RClow, ES_equal, feas) 
CE.equal.CP.prof <- subset(CE.equal.CP, CE.equal.CP$NPV_25_4_CPhigh_RClow>0)
CE.equal.CP.loss <- subset(CE.equal.CP, CE.equal.CP$NPV_25_4_CPhigh_RClow<=0)
CE.equal.CP.prof$CE_equal_CPhigh <- CE.fun.profit(NPV=CE.equal.CP.prof$NPV_25_4_CPhigh_RClow, ES=CE.equal.CP.prof$ES_equal, F=CE.equal.CP.prof$feas)
CE.equal.CP.loss$CE_equal_CPhigh <- CE.fun.loss(NPV=CE.equal.CP.loss$NPV_25_4_CPhigh_RClow, ES=CE.equal.CP.loss$ES_equal, F=CE.equal.CP.loss$feas)
CE.equal.CP.prof2 <- CE.equal.CP.prof[,-c(2:4)]
CE.equal.CP.loss2 <- CE.equal.CP.loss[,-c(2:4)]
#CE.equal.CP2 <- bind_rows(CE.equal.CP.loss2, CE.equal.CP.prof2) %>% as.data.frame
CE4 <- right_join(CE2, CE.equal.CP.prof2, by="FID_site")%>% as.data.frame
CE4$CE_equal_CPhigh_rank <- rank(-CE4$CE_equal_CPhigh)
dim(CE4) #check no of profitable sites

##biodiversity high weighting
CE.BD.CP <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_RClow, ES_BD, feas) 
CE.BD.CP.prof <- subset(CE.BD.CP, CE.BD.CP$NPV_25_4_CPhigh_RClow>0)
CE.BD.CP.loss <- subset(CE.BD.CP, CE.BD.CP$NPV_25_4_CPhigh_RClow<=0)
CE.BD.CP.prof$CE_BD_CPhigh <- CE.fun.profit(NPV=CE.BD.CP.prof$NPV_25_4_CPhigh_RClow, ES=CE.BD.CP.prof$ES_BD, F=CE.BD.CP.prof$feas)
CE.BD.CP.loss$CE_BD_CPhigh <- CE.fun.loss(NPV=CE.BD.CP.loss$NPV_25_4_CPhigh_RClow, ES=CE.BD.CP.loss$ES_BD, F=CE.BD.CP.loss$feas)
CE.BD.CP.prof2 <- CE.BD.CP.prof[,-c(2:4)]
CE.BD.CP.loss2 <- CE.BD.CP.loss[,-c(2:4)]
#CE.BD.CP2 <- bind_rows(CE.BD.CP.loss2, CE.BD.CP.prof2) %>% as.data.frame
CE4 <- right_join(CE4, CE.BD.CP.prof2, by="FID_site")%>% as.data.frame
CE4$CE_BD_CPhigh_rank <- rank(-CE4$CE_BD_CPhigh)
head(CE4)

##fisheries high weighting
CE.fish.CP <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_RClow, ES_fish, feas) 
CE.fish.CP.prof <- subset(CE.fish.CP, CE.fish.CP$NPV_25_4_CPhigh_RClow>0)
CE.fish.CP.loss <- subset(CE.fish.CP, CE.fish.CP$NPV_25_4_CPhigh_RClow<=0)
CE.fish.CP.prof$CE_fish_CPhigh <- CE.fun.profit(NPV=CE.fish.CP.prof$NPV_25_4_CPhigh_RClow, ES=CE.fish.CP.prof$ES_fish, F=CE.fish.CP.prof$feas)
CE.fish.CP.loss$CE_fish_CPhigh <- CE.fun.loss(NPV=CE.fish.CP.loss$NPV_25_4_CPhigh_RClow, ES=CE.fish.CP.loss$ES_fish, F=CE.fish.CP.loss$feas)
CE.fish.CP.prof2 <- CE.fish.CP.prof[,-c(2:4)]
CE.fish.CP.loss2 <- CE.fish.CP.loss[,-c(2:4)]
#CE.fish.CP2 <- bind_rows(CE.fish.CP.loss2, CE.fish.CP.prof2) %>% as.data.frame
CE4 <- right_join(CE4, CE.fish.CP.prof2, by="FID_site")%>% as.data.frame
CE4$CE_fish_CPhigh_rank <- rank(-CE4$CE_fish_CPhigh)
dim(CE4)

##DIN removal high weighting
CE.DIN.CP <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_RClow, ES_DIN, feas) 
CE.DIN.CP.prof <- subset(CE.DIN.CP, CE.DIN.CP$NPV_25_4_CPhigh_RClow>0)
CE.DIN.CP.loss <- subset(CE.DIN.CP, CE.DIN.CP$NPV_25_4_CPhigh_RClow<=0)
CE.DIN.CP.prof$CE_DIN_CPhigh <- CE.fun.profit(NPV=CE.DIN.CP.prof$NPV_25_4_CPhigh_RClow, ES=CE.DIN.CP.prof$ES_DIN, F=CE.DIN.CP.prof$feas)
CE.DIN.CP.loss$CE_DIN_CPhigh <- CE.fun.loss(NPV=CE.DIN.CP.loss$NPV_25_4_CPhigh_RClow, ES=CE.DIN.CP.loss$ES_DIN, F=CE.DIN.CP.loss$feas)
CE.DIN.CP.prof2 <- CE.DIN.CP.prof[,-c(2:4)]
CE.DIN.CP.loss2 <- CE.DIN.CP.loss[,-c(2:4)]
#CE.DIN.CP2 <- bind_rows(CE.DIN.CP.loss2, CE.DIN.CP.prof2) %>% as.data.frame
CE4 <- right_join(CE4, CE.DIN.CP.prof2, by="FID_site")%>% as.data.frame
CE4$CE_DIN_CPhigh_rank <- rank(-CE4$CE_DIN_CPhigh)
head(CE4)

##flood mitigation high weighting
CE.flood.CP <- CE2 %>% 
  dplyr::select(FID_site, NPV_25_4_CPhigh_RClow, ES_flood, feas) 
CE.flood.CP.prof <- subset(CE.flood.CP, CE.flood.CP$NPV_25_4_CPhigh_RClow>0)
CE.flood.CP.loss <- subset(CE.flood.CP, CE.flood.CP$NPV_25_4_CPhigh_RClow<=0)
CE.flood.CP.prof$CE_flood_CPhigh <- CE.fun.profit(NPV=CE.flood.CP.prof$NPV_25_4_CPhigh_RClow, ES=CE.flood.CP.prof$ES_flood, F=CE.flood.CP.prof$feas)
CE.flood.CP.loss$CE_flood_CPhigh <- CE.fun.loss(NPV=CE.flood.CP.loss$NPV_25_4_CPhigh_RClow, ES=CE.flood.CP.loss$ES_flood, F=CE.flood.CP.loss$feas)
CE.flood.CP.prof2 <- CE.flood.CP.prof[,-c(2:4)]
CE.flood.CP.loss2 <- CE.flood.CP.loss[,-c(2:4)]
#CE.flood.CP2 <- bind_rows(CE.flood.CP.loss2, CE.flood.CP.prof2) %>% as.data.frame
CE4 <- right_join(CE4, CE.flood.CP.prof2, by="FID_site")%>% as.data.frame
CE4$CE_flood_CPhigh_rank <- rank(-CE4$CE_flood_CPhigh)
dim(CE4)

#save to do cluster analysis in figures
write.csv(CE3, file="CE3.df.CPlow.csv", row.names = T)
write.csv(CE4, file="CE4.df.CPhigh.csv", row.names = T)

# Comparison of priority approaches
#do subset of highest rankings for each priority approach and calculate benefits
options(scipen = 999)
## carbon abatement
CE2$abate_CO2eMg_25yrs_rank <- rank(-CE2$abate_CO2eMg_25yrs)
which.min(CE2$abate_CO2eMg_25yrs_rank) #matches to rank 1
which.max(CE2$abate_CO2eMg_25yrs)
C20 <- subset(CE2, CE2$abate_CO2eMg_25yrs_rank<=10)
C20.2 <- C20 %>% dplyr::select(Area_ha, NPV="NPV_25_4_CPlow_RClow", abate_CO2eMg_25yrs, feas, ES_equal)
C20.sum <- apply(C20.2, 2, sum) %>% as.data.frame
colnames(C20.sum)[1]<-"carbon_abate_25yrs" 
C20.ranks <- with(C20, data.frame(FID_site, abate_CO2eMg_25yrs_rank, BASIN_NAME))
C20.order <- C20.ranks[order(C20.ranks$abate_CO2eMg_25yrs_rank),]
mean(C20.2$feas)

## ecosystem services equal
CE2$ES_equal_rank <- rank(-CE2$ES_equal)
which.max(CE2$ES_equal) #matches to rank 1
which.min(CE2$ES_equal_rank)
ES20 <- subset(CE2, CE2$ES_equal_rank<=10)
ES20.2 <- ES20 %>% dplyr::select(Area_ha, NPV="NPV_25_4_CPlow_RClow", abate_CO2eMg_25yrs, feas, ES_equal)
ES20.sum <- apply(ES20.2, 2, sum) %>% as.data.frame
colnames(ES20.sum)[1]<-"ES_equal" 
ES20.ranks <- with(ES20, data.frame(FID_site, ES_equal_rank, BASIN_NAME))
ES20.order <- ES20.ranks[order(ES20.ranks$ES_equal_rank),]
mean(ES20.2$feas)

## NPV_25_4_CPlow_RClow
CE2$NPV_25_4_CPlow_RClow_rank <- rank(-CE2$NPV_25_4_CPlow_RClow) #rank from highest positive to lowest negative
which.min(CE2$NPV_25_4_CPlow_RClow_rank) #matches to rank 1
which.max(CE2$NPV_25_4_CPlow_RClow_rank)
which.max(CE2$NPV_25_4_CPlow_RClow)
NPV20 <- subset(CE2, CE2$NPV_25_4_CPlow_RClow_rank<=10)
NPV20.2 <- NPV20 %>% dplyr::select(Area_ha, NPV="NPV_25_4_CPlow_RClow", abate_CO2eMg_25yrs, feas, ES_equal)
NPV20.sum <- apply(NPV20.2, 2, sum) %>% as.data.frame
colnames(NPV20.sum)[1]<-"NPV_25_4_CPlow_RClow" 
NPV20.ranks <- with(NPV20, data.frame(FID_site, NPV_25_4_CPlow_RClow_rank, BASIN_NAME))
NPV20.order <- NPV20.ranks[order(NPV20.ranks$NPV_25_4_CPlow_RClow_rank),]

## NPV_25_4_CPhigh_RClow
CE2$NPV_25_4_CPhigh_RClow_rank <- rank(-CE2$NPV_25_4_CPhigh_RClow)
which.min(CE2$NPV_25_4_CPhigh_RClow_rank) #matches to rank 1
which.max(CE2$NPV_25_4_CPhigh_RClow)
NPV20.CPhigh <- subset(CE2, CE2$NPV_25_4_CPhigh_RClow_rank<=10)
NPV20.CPhigh.2 <- NPV20.CPhigh %>% dplyr::select(Area_ha, NPV="NPV_25_4_CPhigh_RClow", abate_CO2eMg_25yrs, feas, ES_equal)
NPV20.CPhigh.sum <- apply(NPV20.CPhigh.2, 2, sum) %>% as.data.frame
colnames(NPV20.CPhigh.sum)[1]<-"NPV_25_4_CPhigh_RClow" 
NPV20.CPhigh.ranks <- with(NPV20.CPhigh, data.frame(FID_site, NPV_25_4_CPhigh_RClow_rank, BASIN_NAME))
NPV20.CPhigh.order <- NPV20.CPhigh.ranks[order(NPV20.CPhigh.ranks$NPV_25_4_CPhigh_RClow_rank),]

## CE with ES equal CP low
CE.equal20 <- subset(CE3, CE3$CE_equal_rank<=10)
CE.equal20.2 <- CE.equal20 %>% dplyr::select(Area_ha, NPV="NPV_25_4_CPlow_RClow", abate_CO2eMg_25yrs, feas, ES_equal)
CE.equal20.sum <- apply(CE.equal20.2, 2, sum) %>% as.data.frame
colnames(CE.equal20.sum)[1] <- "CE_equal"
CE.equal20.ranks <- with(CE.equal20, data.frame(FID_site, CE_equal_rank, BASIN_NAME))
CE.equal20.order <- CE.equal20.ranks[order(CE.equal20.ranks$CE_equal_rank),]

## CE with ES equal CP high
CE.equal20.CPhigh <- subset(CE4, CE4$CE_equal_CPhigh_rank<=10)
CE.equal20.CPhigh.2 <- CE.equal20.CPhigh %>% dplyr::select(Area_ha, NPV="NPV_25_4_CPhigh_RClow", abate_CO2eMg_25yrs, feas, ES_equal)
CE.equal20.CPhigh.sum <- apply(CE.equal20.CPhigh.2, 2, sum) %>% as.data.frame
colnames(CE.equal20.CPhigh.sum)[1] <- "CE_equal_CPhigh"
CE.equal20.CPhigh.ranks <- with(CE.equal20.CPhigh, data.frame(FID_site, CE_equal_CPhigh_rank, BASIN_NAME))
CE.equal20.CPhigh.order <- CE.equal20.CPhigh.ranks[order(CE.equal20.CPhigh.ranks$CE_equal_CPhigh_rank),]


## combine tables
benefits.table <- cbind(C20.sum, ES20.sum, NPV20.sum, NPV20.CPhigh.sum, CE.equal20.sum, CE.equal20.CPhigh.sum, 
                        rownames=T)

colnames(benefits.table) <- c("C20", "ES20","NPV20","NPV20.CPhigh", "CEequal20",  
                              "CEequal20.CPhigh")
write.csv(benefits.table, file="benefits.top10.sum.csv", row.names=T)

ranks.table <- cbind(C20.order, ES20.order, NPV20.order, NPV20.CPhigh.order, CE.equal20.order,  
                     CE.equal20.CPhigh.order)
write.csv(ranks.table, file="benefits.top10.ranks.csv", row.names=F)


#Plot NPV vs CE
CE3$NPV_25_4_CPlow_RClow_rank <- rank(-CE3$NPV_25_4_CPlow_RClow) #rank from highest positive to lowest negative
CE4$NPV_25_4_CPhigh_RClow_rank <- rank(-CE4$NPV_25_4_CPhigh_RClow) #rank from highest positive to lowest negative

tiff("NVP.CE.fig.tif", units="mm", width=120, height=120, res=800, compression = "lzw")
par(mfrow=c(2,1), mar=c(5,5,1,2))
with(CE3, plot(CE_equal_rank ~ NPV_25_4_CPlow_RClow_rank, xlab = "NPV site rank (S1)", ylab="CE site rank (S1)", cex.lab=1))
mtext("a", line=0, side=3, adj=0, cex=1.5)
with(CE4, plot(CE_equal_CPhigh_rank ~ NPV_25_4_CPhigh_RClow_rank, xlab = "NPV site rank (S3)", ylab="CE site rank (S3)", cex.lab=1))
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()


# Cluster analysis and nMDS
# Create matrix 
CE3.matrix <- CE3 %>% 
  dplyr::select("FID_site", "CE_equal_rank", "CE_BD_rank", "CE_fish_rank", "CE_DIN_rank", "CE_flood_rank") 
dim(CE3.matrix)
CE4.matrix <- CE4 %>% 
  dplyr::select("FID_site", "CE_equal_CPhigh_rank", "CE_BD_CPhigh_rank", "CE_fish_CPhigh_rank", "CE_DIN_CPhigh_rank", "CE_flood_CPhigh_rank") 
dim(CE4.matrix)

CE.matrix <- left_join(CE3.matrix, CE4.matrix, by="FID_site") %>% as.matrix
dim(CE.matrix)
colnames(CE.matrix)
CE.matrix <- CE.matrix[,-1]
rownames(CE.matrix) <- c(1:200) #add row names for FID sites #not true FID

#However have different number of sites - 278 for S1 and 200 for S3, so can't put them on same ordination
#Take dataset using CP high scenario (S3)
CE4.matrix <- CE4.matrix[,-1] %>% as.matrix
dim(CE4.matrix)
rownames(CE4.matrix) <- c(1:278) #add row names for FID sites

# create dissimilarity matrix - all columns add to 1
library(vegan)
CE.d.matrix <- prop.table(CE4.matrix, margin=2)
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
CE.palette <- brewer.pal(5, "Paired")
display.brewer.pal(5, "Paired")
CE.names.CPhigh <- colnames(CE4.matrix)
CE.labels.CPhigh <- c("S3 equal", "S3 BD-high", "S3 fish-high", "S3 DIN-high", "S3 flood-high")

# plot dendogram and ordination of CE site ranks across different scenarios
tiff("fig.CE.dendo.ord.tif", units="mm", width=190, height=95, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,2))
plot(hc.CE.jac.t, labels=CE.labels.CPhigh, xlab = "Scenario", ylab="Dissimilarity", cex.lab=1, cex=1, main="")
mtext("a", line=0, side=3, adj=0, cex=1.5)
plot(CE.t.mds$points, type='n', xlab='NMDS1', ylab='NMDS2')
points(CE.t.mds$points, col=CE.palette, pch=19)
legend(x=-0.02, y=-0.001, CE.labels.CPhigh, 
       fill=CE.palette, ncol=1, #x.intersp=0.5, y.intersp=1,
       cex=1, bty="n") #text.width=0, xjust=0.5)
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()

#plot dendogram only
tiff("fig.CE.dendo.tif", units="mm", width=95, height=95, res=800, compression = "lzw")
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(hc.CE.jac.t, labels=CE.labels.CPhigh, xlab = "EP Scenario", ylab="Dissimilarity", cex.lab=1, cex=0.8, main="")
mtext("C", line=0, side=3, adj=0, cex=1.2)
dev.off()

# plot - CE score and area
tiff("CE.area.fig.tif", units="mm", width=190, height=120, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(CE_equal~Area_ha, data=CE3, xlab="Area (ha)", ylab="EP score (S1)", xlim=c(0,5100))
mtext("a", line=0, side=3, adj=0, cex=1.5)
plot(CE_equal_CPhigh~Area_ha, data=CE4, xlab="Area (ha)", ylab="EP score (S3)", xlim=c(0,5100))
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()

tiff("NPV.area.fig.tif", units="mm", width=190, height=120, res=800, compression = "lzw")
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(NPV_25_4_CPlow_RClow~Area_ha, data=CE2, xlab="Area (ha)", ylab="NPV AU$ 25 years", xlim=c(0,5100))
mtext("a", line=0, side=3, adj=0, cex=1.5)
plot(NPV_25_4_CPhigh_RClow~Area_ha, data=CE2, xlab="Area (ha)", ylab="NPV AU$ 25 years", xlim=c(0,5100))
mtext("b", line=0, side=3, adj=0, cex=1.5)
dev.off()


#Exclude CYC no go areas
table(CE$CYC_site) #30 sites contain CYC populations
CE.nogo <- subset(CE, CE$CYC_site==0)
dim(CE.nogo)
sum(CE$Area_ha)
sum(CE.nogo$Area_ha) #area reduced from 31686 ha to 13873 ha
#also do this in ArcGIS for map

#Identify area with high ES values (>mean) from restorable area
mean <- mean(CE.nogo$ES_equal)
EShigh <- subset(CE.nogo, CE.nogo$ES_equal>mean)
sum(EShigh$Area_ha)

#Identify restoration sites with opportunities for TO-led restoration or partnerships
range(CE2$parties_ha) #some NAs - sites with no registered Aboriginal parties
which(is.na(CE2$parties_ha)) #only 6 sites
range(na.omit(CE2$parties_ha)) #take sites which has any proportion/ha with a party, so >0
range(CE2$non_exclusive_ha)
range(na.omit(CE2$non_exclusive_ha))
CE2$parties_ha[is.na(CE2$parties_ha)] <- 0
CE2$non_exclusive_ha[is.na(CE2$non_exclusive_ha)] <- 0
hist(na.omit(CE2$non_exclusive_ha), breaks=100) #most sites small, take sites >10 ha
range(CE2$tenure_ha)
range(na.omit(CE2$tenure_ha))
CE2$tenure_ha[is.na(CE2$tenure_ha)] <- 0
hist(na.omit(CE2$tenure_ha), breaks=100)#most sites small, take sites >10 ha

CE2$native_title <- ifelse(CE2$non_exclusive_ha>=10 & CE2$parties_ha>0, 1, 0)
CE2$native_title_potential <- ifelse(CE2$tenure_ha>=10 & CE2$parties_ha>0, 1, 0)
which(CE2$native_title==1)
which(CE2$native_title_potential==1) #many sites overlap so combine function
CE2$TO_opportunity <- ifelse((CE2$non_exclusive_ha>=10 | CE2$tenure_ha>=10) & CE2$parties_ha>0, 1, 0)
which(CE2$TO_opportunity==1)
table(CE2$TO_opportunity) #12 sites with potential for TO-led or partnerships
CE.TO <- subset(CE2, CE2$TO_opportunity==1)
dim(CE.TO)
sum(CE.TO$Area_ha) #5548 ha - includes some really big sites - FID 328, 354 and 359
table(CE.TO$name)

sum(CE2$non_exclusive_ha)
sum(CE2$tenure_ha)

#--- INVESTIGATE TRADE-OFFS AND SYNERGIES AMONG CARBON AND CO-BENEFITS

colnames(CE2) 
#carbon abatement per year
CE2$abate_CO2eMg_yr <- CE2$abate_CO2eMg_25yrs/25
hist(CE2$abate_CO2eMg_yr, breaks = 100)
hist(log(CE2$abate_CO2eMg_yr), breaks = 100) #log to get normal distribution
hist(CE2$abate_CO2eMg_25yrs, breaks = 100)
hist(log(CE2$abate_CO2eMg_25yrs), breaks = 100) #log to get normal distribution

#save dataframes - with and without CYC nogo areas
write.csv(CE, file="CE.df.all.csv", row.names = T)
write.csv(CE2, file="CE.df.nogo.csv", row.names = T)

#go with logged total carbon abatement per site over 25 years
ben.df <- CE2 %>% dplyr::select(abate_CO2eMg_yr, 
                               Near_wetland_m, Near_ramsar_m, EVNT_n, CYChab_ha,
                               Near_3order_m, Near_FHA_m, low_tidal_ha, 
                               DIN_load, TSS_load, intersect_3order_ha,
                               Fitzroy_100m_100yr_peak, mang_ha, 
                               wetland_dist_scale, ramsar_dist_scale, EVNT_n_scale, CYC_hab_scale,
                               dist_3order_scale, dist_FHA_scale, low_tidal_scale, 
                               DIN_load_scale, TSS_load_scale, intersect_3order_scale,
                               Fitzroy_100yr_scale, mang_ha_scale, Area_ha) 
head(ben.df)

#check frequencies
hist(ben.df$abate_CO2eMg_yr)
range(ben.df$abate_CO2eMg_yr)
hist(log(ben.df$abate_CO2eMg_yr)) #log transform
hist(ben.df$Near_wetland_m)
range(ben.df$Near_wetland_m) #zeros so can't log
hist(log(ben.df$Near_wetland_m+10)) #doesn't improve, so keep normal
hist(ben.df$Near_ramsar_m)
range(ben.df$Near_ramsar_m) #zeros so can't log
hist(sqrt(ben.df$Near_ramsar_m)) #keep normal
hist(ben.df$EVNT_n)
range(ben.df$EVNT_n) #zeros so can't log
hist(log(ben.df$EVNT_n+10)) #doesn't improve, keep normal
hist(ben.df$CYChab_ha)
range(ben.df$CYChab_ha) #zeros so can't log
hist(log(ben.df$CYChab_ha+10)) #doesn't improve, keep normal
hist(ben.df$Near_3order_m)
range(ben.df$Near_3order_m) #zeros so can't log
hist(log(ben.df$Near_3order_m+10)) #log+10 transform
hist(ben.df$Near_FHA_m)
range(ben.df$Near_FHA_m) #zeros so can't log
hist(log(ben.df$Near_FHA_m+10)) #log+10 transform
hist(ben.df$low_tidal_ha)
range(ben.df$low_tidal_ha) #zeros so can't log
hist(log(ben.df$low_tidal_ha+10)) #doesn't improve, keep normal
hist(ben.df$DIN_load)
range(ben.df$DIN_load) #zeros so can't log
hist(sqrt(ben.df$DIN_load)) #don't transform
hist(ben.df$TSS_load, breaks=100)
range(ben.df$TSS_load) #zeros so can't log
hist(sqrt(ben.df$TSS_load), breaks=100) #don't transform
hist(ben.df$intersect_3order_ha)
range(ben.df$intersect_3order_ha) #zeros so can't log
hist(log(ben.df$intersect_3order_ha+10)) #don't transform
hist(ben.df$Fitzroy_100m_100yr_peak)
range(ben.df$Fitzroy_100m_100yr_peak) #zeros so can't log
hist(sqrt(ben.df$Fitzroy_100m_100yr_peak)) #don't transform
hist(ben.df$mang_ha)
range(ben.df$mang_ha) #zeros so can't log
hist(sqrt(ben.df$mang_ha)) #don't transform

#make some pre-transformed variables
ben.df$log.abate_CO2eMg_yr <- log(ben.df$abate_CO2eMg_yr)

ben.df.raw <- ben.df %>% dplyr::select (log.abate_CO2eMg_yr, 
                             Near_wetland_m, Near_ramsar_m, EVNT_n, CYChab_ha,
                             Near_3order_m, Near_FHA_m, low_tidal_ha, 
                             DIN_load, TSS_load, intersect_3order_ha,
                             Fitzroy_100m_100yr_peak, mang_ha)
ben.df.scale <- ben.df %>% dplyr::select (log.abate_CO2eMg_yr, 
                                          wetland_dist_scale, ramsar_dist_scale, EVNT_n_scale, CYC_hab_scale,
                                          dist_3order_scale, dist_FHA_scale, low_tidal_scale, 
                                          DIN_load_scale, TSS_load_scale, intersect_3order_scale,
                                          Fitzroy_100yr_scale, mang_ha_scale)

#pearson correlation coefficient
library("Hmisc")
ben.rcorr = rcorr(as.matrix(ben.df.raw), type = "pearson") #generates one output with several tables
ben.coeff = ben.rcorr$r
write.csv(ben.coeff, file="correlation.matrix.benefits.csv", row.names = T)

ben.rcorr.scale = rcorr(as.matrix(ben.df.scale), type = "pearson") #generates one output with several tables
ben.coeff.scale = ben.rcorr.scale$r
write.csv(ben.coeff.scale, file="correlation.matrix.benefits.scaled.csv", row.names = T)

#pairs plot
library(psych)
tiff("pairsplot.benefits.tif", units="mm", width=200, height=200, res=800, compression = "lzw")
pairs.panels(ben.df.scale, 
             method = "pearson", # correlation method
             hist.col = "grey",
             density = FALSE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             lm = TRUE,
             labels=c("carbon log", "wetland dist", "Ramsar dist", "EVNT spp", "chat habitat", "watercourse dist", "FHA dist", "low intertidal",
                      "DIN load", "TSS load", "watercourse area", "flood zone", "mangrove"),
             cex.cor = 1,
             scale=FALSE,
             gap=0,
             #stars = FALSE, #show the significance of correlations
             main="")
dev.off()

tiff("pairsplot.benefits.raw.tif", units="mm", width=200, height=200, res=800, compression = "lzw")
pairs.panels(ben.df.raw, 
             method = "pearson", # correlation method
             hist.col = "grey",
             density = FALSE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             lm = TRUE,
             labels=c("carbon log", "wetland dist", "Ramsar dist", "EVNT spp", "chat habitat", "stream dist", "FHA dist", "low intertidal",
                      "DIN conc", "TSS conc", "stream area", "flood zone", "mangrove"),
             cex.cor = 1,
             scale=FALSE,
             gap=0,
             #stars = FALSE, #show the significance of correlations
             main="")
dev.off()


##Analyse the relationships between carbon abatement and cobenefits, using separate linear models for each cobenefit measure and region. 
##Assess relationships with Pearson correlation coefficients. 
##Analyse regions (north Qld, south west WA, north WA) separately because they differ substantially in biodiversity and environmental conditions

tiff("benefits.area.fig.tif", units="mm", width=200, height=140, res=800, compression = "lzw")
par(mfrow=c(2,3), mar=c(4,4,2,1), mgp=c(2,0.5,0), tck=-0.05)

#hist(sqrt(ben.df$EVNT_n))
m.c.evnt <- lm(log(abate_CO2eMg_yr) ~ EVNT_n, data=ben.df)
summary(m.c.evnt) 
plot(log(abate_CO2eMg_yr)~EVNT_n, data=ben.df, xlab="Threatened species (abundance)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.evnt)
cor(log(ben.df$abate_CO2eMg_yr), ben.df$EVNT_n, method = 'pearson')
mtext("a", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.13", line=0.5, side=3, adj=1, cex=1) 

#hist(sqrt(ben.df$CYChab_ha))
m.c.cyc <- lm(log(abate_CO2eMg_yr) ~ sqrt(CYChab_ha), data=ben.df)
summary(m.c.cyc)
plot(log(abate_CO2eMg_yr)~sqrt(CYChab_ha), data=ben.df, xlab="Capricorn Yellow Chat habitat (ha) [sqrt]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.cyc)
cor(log(ben.df$abate_CO2eMg_yr), sqrt(ben.df$CYC_ha), method = 'pearson')
mtext("b", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.26", line=0.5, side=3, adj=1, cex=1) 

#hist(sqrt(ben.df$low_tidal_ha))
m.c.lowtidal <- lm(log(abate_CO2eMg_yr) ~ sqrt(low_tidal_ha), data=ben.df)
summary(m.c.lowtidal) 
plot(log(abate_CO2eMg_yr)~sqrt(low_tidal_ha), data=ben.df, xlab="Low intertidal area (ha) [sqrt]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.lowtidal)
cor(log(ben.df$abate_CO2eMg_yr), sqrt(ben.df$low_tidal_ha), method = 'pearson')
mtext("c", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.23", line=0.5, side=3, adj=1, cex=1) 

#hist(sqrt(ben.df$intersect_3order_ha))
m.c.3orderint <- lm(log(abate_CO2eMg_yr) ~ sqrt(intersect_3order_ha), data=ben.df)
summary(m.c.3orderint)
plot(log(abate_CO2eMg_yr)~sqrt(intersect_3order_ha), data=ben.df, xlab="Major watercouse area (ha) [sqrt]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.3orderint) 
cor(log(ben.df$abate_CO2eMg_yr), sqrt(ben.df$intersect_3order_ha), method = 'pearson')
mtext("d", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.40", line=0.5, side=3, adj=1, cex=1) 

#hist(sqrt(ben.df$Fitzroy_100m_100yr_peak))
m.c.peak <- lm(log(abate_CO2eMg_yr) ~ sqrt(Fitzroy_100m_100yr_peak), data=ben.df)
summary(m.c.peak) 
plot(log(abate_CO2eMg_yr)~sqrt(Fitzroy_100m_100yr_peak), data=ben.df, xlab="1% AEP flood (ha) [sqrt]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.peak)
cor(log(ben.df$abate_CO2eMg_yr), sqrt(ben.df$Fitzroy_100m_100yr_peak), method = 'pearson')
mtext("e", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.32", line=0.5, side=3, adj=1, cex=1) 

#hist(sqrt(ben.df$mang_ha))
m.c.mang <- lm(log(abate_CO2eMg_yr) ~ sqrt(mang_ha), data=ben.df)
summary(m.c.mang) 
plot(log(abate_CO2eMg_yr)~sqrt(mang_ha), data=ben.df, xlab="Historic mangrove area (ha) [sqrt]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.mang)
cor(log(ben.df$abate_CO2eMg_yr), sqrt(ben.df$mang_ha), method = 'pearson')
mtext("f", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.21", line=0.5, side=3, adj=1, cex=1) 

dev.off()


tiff("benefits.dist.fig.tif", units="mm", width=200, height=200, res=800, compression = "lzw")
par(mfrow=c(3,3), mar=c(4,4,2,1), mgp=c(2,0.5,0), tck=-0.05)

ben.df$Near_wetland_km <- ben.df$Near_wetland_m/1000
m.c.wet <- lm(log(abate_CO2eMg_yr) ~ Near_wetland_km, data=ben.df)
summary(m.c.wet) 
plot(log(abate_CO2eMg_yr)~Near_wetland_km, data=ben.df, xlab="Existing wetland distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]" ) #take ECI
abline(m.c.wet)
cor(log(ben.df$abate_CO2eMg_yr), ben.df$Near_wetland_km, method = 'pearson')
mtext("a", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.17", line=0.5, side=3, adj=1, cex=1) #add pearson

ben.df$Near_ramsar_km <- ben.df$Near_ramsar_m/1000
m.c.ram <- lm(log(abate_CO2eMg_yr) ~ Near_ramsar_km, data=ben.df)
summary(m.c.ram) 
plot(log(abate_CO2eMg_yr)~Near_ramsar_km, data=ben.df, xlab="Ramsar wetland distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.ram)
cor(log(ben.df$abate_CO2eMg_yr), ben.df$Near_ramsar_km, method = 'pearson')
mtext("b", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.07", line=0.5, side=3, adj=1, cex=1) 

ben.df$Near_3order_km <- ben.df$Near_3order_m/1000
m.c.3order <- lm(log(abate_CO2eMg_yr) ~ Near_3order_km, data=ben.df)
summary(m.c.3order)
plot(log(abate_CO2eMg_yr)~Near_3order_km, data=ben.df, xlab="Major watercourse distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.3order)
cor(log(ben.df$abate_CO2eMg_yr), ben.df$Near_3order_km, method = 'pearson')
mtext("c", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.05", line=0.5, side=3, adj=1, cex=1) 

ben.df$Near_FHA_km <- ben.df$Near_FHA_m/1000
m.c.fha <- lm(log(abate_CO2eMg_yr) ~ Near_FHA_km, data=ben.df)
summary(m.c.fha)
plot(log(abate_CO2eMg_yr)~Near_FHA_km, data=ben.df, xlab="Fish Habitat Area distance (km)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.fha)
cor(log(ben.df$abate_CO2eMg_yr), ben.df$Near_FHA_km, method = 'pearson')
mtext("d", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.10", line=0.5, side=3, adj=1, cex=1) 

m.c.din <- lm(log(abate_CO2eMg_yr) ~ DIN_load, data=ben.df)
summary(m.c.din) 
plot(log(abate_CO2eMg_yr)~DIN_load, data=ben.df, xlab="DIN catchment concentration (Mg L-1)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.din)
cor(log(ben.df$abate_CO2eMg_yr), ben.df$DIN_load, method = 'pearson')
mtext("e", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.28", line=0.5, side=3, adj=1, cex=1) 

m.c.TSS <- lm(log(abate_CO2eMg_yr) ~ TSS_load, data=ben.df)
summary(m.c.TSS)
plot(log(abate_CO2eMg_yr)~TSS_load, data=ben.df, xlab="TSS catchment concentration (Kt L-1)", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.TSS) 
cor(log(ben.df$abate_CO2eMg_yr), ben.df$TSS_load, method = 'pearson')
mtext("f", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.09", line=0.5, side=3, adj=1, cex=1) 

# but also very correlated with area of the site
#hist(log(ben.df$Area_ha))
m.c.area <- lm(abate_CO2eMg_yr ~ log(Area_ha), data=ben.df)
summary(m.c.area) 
plot(abate_CO2eMg_yr~log(Area_ha), data=ben.df, xlab="Restoration area (ha) [log]", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.area)
cor(log(ben.df$abate_CO2eMg_yr), log(ben.df$Area_ha), method = 'pearson')
mtext("g", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.70", line=0.5, side=3, adj=1, cex=1) #add r

dev.off()


#scaled
tiff("benefits.scaled.fig.tif", units="mm", width=200, height=300, res=800, compression = "lzw")
par(mfrow=c(4,3), mar=c(4,4,2,1), mgp=c(2,0.5,0), tck=-0.05)
m.c.wet <- lm(log(abate_CO2eMg_yr) ~ wetland_dist_scale, data=ben.df)
summary(m.c.wet) # slope +/- 1 SE = 0.0001040617 +/- 0.0001315590
plot(log(abate_CO2eMg_yr)~wetland_dist_scale, data=ben.df, xlab="Existing wetland distance", ylab="Carbon (Mg CO2-e yr-1) [log]" ) #take ECI
abline(m.c.wet)
mtext("a", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.17 ***", line=0.5, side=3, adj=1, cex=1) #add pearson

m.c.ram <- lm(log(abate_CO2eMg_yr) ~ ramsar_dist_scale, data=ben.df)
summary(m.c.ram) #0.000016601128 +/- 0.000004282103 ***
plot(log(abate_CO2eMg_yr)~ramsar_dist_scale, data=ben.df, xlab="Ramsar distance", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.ram)
mtext("b", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.07 N.S.", line=0.5, side=3, adj=1, cex=1) 

m.c.evnt <- lm(log(abate_CO2eMg_yr) ~ EVNT_n_scale, data=ben.df)
summary(m.c.evnt) #0.1748976 +/- 0.0563045 **
plot(log(abate_CO2eMg_yr)~EVNT_n_scale, data=ben.df, xlab="Threatened species", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.evnt)
mtext("c", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.13 ***", line=0.5, side=3, adj=1, cex=1) 

m.c.cyc <- lm(log(abate_CO2eMg_yr) ~ CYC_hab_scale, data=ben.df)
summary(m.c.cyc)#0.003353241 +/- 0.000462499 ***
plot(log(abate_CO2eMg_yr)~CYC_hab_scale, data=ben.df, xlab="Capricorn Yellow Chat habitat", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.cyc)
mtext("d", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.27 ***", line=0.5, side=3, adj=1, cex=1) 

m.c.3order <- lm(log(abate_CO2eMg_yr) ~ dist_3order_scale, data=ben.df)
summary(m.c.3order)#-0.0002797658  0.0001354409 *
plot(log(abate_CO2eMg_yr)~dist_3order_scale, data=ben.df, xlab="Major watercourse distance", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.3order)
mtext("e", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.05 N.S.", line=0.5, side=3, adj=1, cex=1) 

m.c.fha <- lm(log(abate_CO2eMg_yr) ~ dist_FHA_scale, data=ben.df)
summary(m.c.fha)#0.00004191400 0.00003010917
plot(log(abate_CO2eMg_yr)~dist_FHA_scale, data=ben.df, xlab="Fish Habitat Area distance", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.fha)
mtext("f", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.10 *", line=0.5, side=3, adj=1, cex=1) 

m.c.lowtidal <- lm(log(abate_CO2eMg_yr) ~ low_tidal_scale, data=ben.df)
summary(m.c.lowtidal) #0.06036101 0.01762839 ***
plot(log(abate_CO2eMg_yr)~low_tidal_scale, data=ben.df, xlab="Low intertidal zone", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.lowtidal)
mtext("g", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.17 ***", line=0.5, side=3, adj=1, cex=1) 

m.c.din <- lm(log(abate_CO2eMg_yr) ~ DIN_load_scale, data=ben.df)
summary(m.c.din) #0.0021520369 0.0002357813 ***
plot(log(abate_CO2eMg_yr)~DIN_load_scale, data=ben.df, xlab="DIN catchment concentration", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.din)
mtext("h", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.28 ***", line=0.5, side=3, adj=1, cex=1) 

m.c.TSS <- lm(log(abate_CO2eMg_yr) ~ TSS_load_scale, data=ben.df)
summary(m.c.TSS)
plot(log(abate_CO2eMg_yr)~TSS_load_scale, data=ben.df, xlab="TSS catchment concentration", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.TSS) # 0.0016882077 0.0001872988 ***
mtext("i", line=0.5, side=3, adj=0, cex=1)
mtext("r = -0.08 N.S.", line=0.5, side=3, adj=1, cex=1) 

m.c.3orderint <- lm(log(abate_CO2eMg_yr) ~ intersect_3order_scale, data=ben.df)
summary(m.c.3orderint)
plot(log(abate_CO2eMg_yr)~intersect_3order_scale, data=ben.df, xlab="Major watercouse area", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.3orderint) #0.029315997 0.003802927 ***
mtext("j", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.36 ***", line=0.5, side=3, adj=1, cex=1) 

m.c.peak <- lm(log(abate_CO2eMg_yr) ~ Fitzroy_100yr_scale, data=ben.df)
summary(m.c.peak) #0.0020769760 0.0003391842 ***
plot(log(abate_CO2eMg_yr)~Fitzroy_100yr_scale, data=ben.df, xlab="Flood 100yr zone", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.peak)
mtext("k", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.21 ***", line=0.5, side=3, adj=1, cex=1) 

m.c.mang <- lm(log(abate_CO2eMg_yr) ~ mang_ha_scale, data=ben.df)
summary(m.c.mang) #0.26353468 0.03783638 ***
plot(log(abate_CO2eMg_yr)~mang_ha_scale, data=ben.df, xlab="Historic mangrove area", ylab="Carbon (Mg CO2-e yr-1) [log]")
abline(m.c.mang)
mtext("l", line=0.5, side=3, adj=0, cex=1)
mtext("r = 0.20 ***", line=0.5, side=3, adj=1, cex=1) 

dev.off()

#mean abatement
range(CE$abate_CO2eMg_25yrs)
CE$abate_CO2eMg_yr_ha <- (CE$abate_CO2eMg_25yrs/25)/CE$Area_ha
abate.yr.ha.mean <- mean(CE$abate_CO2eMg_yr_ha)
abate.yr.ha.length <- length(CE$abate_CO2eMg_yr_ha) 
abate.yr.ha.var <- var(CE$abate_CO2eMg_yr_ha)
abate.yr.ha.se <- sqrt(abate.yr.ha.var/abate.yr.ha.length)

#with no go areas
CE2$abate_CO2eMg_yr_ha <- (CE2$abate_CO2eMg_25yrs/25)/CE2$Area_ha
abate.yr.ha.mean2 <- mean(CE2$abate_CO2eMg_yr_ha)
abate.yr.ha.length2 <- length(CE2$abate_CO2eMg_yr_ha) 
abate.yr.ha.var2 <- var(CE2$abate_CO2eMg_yr_ha)
abate.yr.ha.se2 <- sqrt(abate.yr.ha.var/abate.yr.ha.length)
