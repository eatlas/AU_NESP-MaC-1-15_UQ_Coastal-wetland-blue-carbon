# Reads in and manipulates restoration areas and calculates blue carbon - avoided emissions and removals
# Valerie Hagger 13/02/2024
rm(list = ls())
setwd("R:/NESP115-Q4418/R_working_directory/Fitzroy") #sets the working directory

# load packages
library(tidyverse)
library(readxl)


### S1 Removals and Avoided Emissions ###

## No go areas - sites containing known Capricorn Yellow Chat (CYC) populations
# read in excel file from spatial analysis of rest sites within 1 km buffer of CYC sites 
rest.sites.CYCsites <- read_excel("rsites4_CYCsites_1kmbuf.xls", sheet="rsites4_CYCsites_1kmbuf")
colnames(rest.sites.CYCsites)
dim(rest.sites.CYCsites)
range(rest.sites.CYCsites$FID_1) #FID is the FID_sites
rest.sites.CYC <- rest.sites.CYCsites %>% dplyr::select(FID_site="FID", Area_ha, CYC="Count_")
colnames(rest.sites.CYC)
# Remove if Count_==1

# agricultural land uses in Highest Astronomical Tide (HAT)
#Land use area in HAT
options("scipen" = 100, "digits" = 5) # suppress math annotation
# read in excel file from spatial analysis of area of land uses within the HAT from the Fitzroy NRM land use mapping
HAT.lu <- read_excel("tid5dem_fitzroy2_LU.xls", sheet="tid5dem_fitzroy2_LU")
colnames(HAT.lu)
table(HAT.lu$Secondary_)
table(HAT.lu$Primary_20)
# extract agricultural land uses
HAT.ag <- subset(HAT.lu, HAT.lu$Primary_20=="Production from dryland agriculture and plantations" |
                           HAT.lu$Primary_20=="Production from irrigated agriculture and plantations" |
                           HAT.lu$Primary_20=="Production from relatively natural environments" |
                           HAT.lu$Secondary_=="Intensive horticulture" | HAT.lu$Secondary_=="Intensive animal production" |
                   HAT.lu$ALUM_code_=="6.2.2" | HAT.lu$ALUM_code_=="6.5.2")
HAT.lu.total <- sum(HAT.lu$Landuse_ha)
HAT.ag.total <- sum(HAT.ag$Landuse_ha)
grazing <-  (14170.69791+ 21332.95181)/HAT.ag.total
HAT.ag.sec<-with(HAT.ag, tapply(Landuse_ha, Secondary_, sum)) %>% as.matrix
HAT.ag.ter<-with(HAT.ag, tapply(Landuse_ha, Tertiary_2, sum)) %>% as.matrix

#Land use areas in rest sites
# read in excel file from spatial analysis of area of land uses within the rest sites
rest.sites.lu <- read_excel("rsites4_LU.xls", sheet="rsites4_LU")
colnames(rest.sites.lu)
colnames(rest.sites.lu)[2] <- "FID_site"
# identify which ones are no-go areas
rest.sites.lu.CYC <- full_join(rest.sites.lu, rest.sites.CYC, by="FID_site")
rest.sites.lu.nogo <- subset(rest.sites.lu.CYC, rest.sites.lu.CYC$CYC==0)

rest.sites.lu.ter <- with(rest.sites.lu, tapply(LU_ha, Tertiary_2, sum))
rest.sites.lu.nogo.ter <- with(rest.sites.lu.nogo, tapply(LU_ha, Tertiary_2, sum))
#rest.sites.grazing <- sum(rest.sites.lu.sec[2:3])
#rest.sites.forestry <- sum(rest.sites.lu.sec[4:5])

## what percentage of restoration area is ag land within HAT
sum(rest.sites.lu.ter)/sum(HAT.ag.total) #0.87% 
sum(rest.sites.lu.nogo.ter)/sum(HAT.ag.total) #0.38% 
HAT.ag.prop <- prop.table(HAT.ag.sec, margin=2)
colnames(HAT.ag.prop) <-"HAT.prop"
ag.HAT.all <- cbind(HAT.ag.sec, HAT.ag.prop) %>% as.matrix
HAT.ag.prop.LUtot <- (HAT.ag.sec/HAT.lu.total)*100


#preclear Regional Ecosystems (REs) in HAT
# read in excel file of intersect in ArcGIS of tid5dem_fitzroy and preclearRE, dissolved by DBVG1M and add field (BVG_ha) 
HAT.preRE <- read_excel("tid5dem_fitzroy2_preclearRE.xls", sheet="tid5dem_fitzroy2_preclearRE")
table(HAT.preRE$DBVG1M)
HAT.preBVG.sum<-with(HAT.preRE, tapply(RE_ha, DBVG1M, sum)) %>% as.data.frame

# Coastal wetland restoration - removals
# area and proportion of rest areas within basins 
# read in excel file of intersect in ArcGIS (rsites with Fitzroy drainage basins) 
rest.sites.basins <- read_excel("rsites4_basins.xls", sheet="rsites4_basins") #update with file name
head(rest.sites.basins)
colnames(rest.sites.basins)[2]<-"FID_site" 
rest.sites.basins.ha <- with(rest.sites.basins, tapply(Area_ha, BASIN_NAME, sum)) %>% as.data.frame()
colnames(rest.sites.basins.ha)[1]<-"Area_ha" 
rest.sites.basins.ha$prop <- rest.sites.basins.ha$Area_ha/sum(rest.sites.basins.ha$Area_ha)
write.csv(rest.sites.basins.ha, file="rest_sites_basins_area_prop.csv", row.names=T)
sum(rest.sites.basins.ha$Area_ha)


#remove CYC sites
rest.sites.CYC2 <- rest.sites.CYC %>% select(FID_site, CYC)
rest.sites.basins.CYC <- full_join(rest.sites.basins, rest.sites.CYC2, by="FID_site")
dim(rest.sites.basins) #455
rest.sites.basins.nogo <- subset(rest.sites.basins.CYC, rest.sites.basins.CYC$CYC==0)
dim(rest.sites.basins.nogo) #425
rest.sites.basins.nogo.ha <- with(rest.sites.basins.nogo, tapply(Area_ha, BASIN_NAME, sum)) %>% as.data.frame()
colnames(rest.sites.basins.nogo.ha)[1]<-"Area_ha" 
rest.sites.basins.nogo.ha$prop <- rest.sites.basins.nogo.ha$Area_ha/sum(rest.sites.basins.nogo.ha$Area_ha)
sum(rest.sites.basins.nogo.ha$Area_ha)

# number and proportion of rest sites within basins
rest.sites.basins.n <- table(rest.sites.basins$BASIN_NAME) %>% as.matrix
colnames(rest.sites.basins.n)[1]<-"Freq" 
rest.sites.basins.n.prop <- prop.table(rest.sites.basins.n)
rest.sites.basins.n <- cbind(rest.sites.basins.n, rest.sites.basins.n.prop)
colnames(rest.sites.basins.n)[2]<-"Prop" 
write.csv(rest.sites.basins.n, file="rest_sites_basins_number_prop.csv", row.names=T)

#size of sites
hist(rest.sites.basins.nogo$Area_ha, breaks=200)
range(rest.sites.basins.nogo$Area_ha)
dim(subset(rest.sites.basins.nogo, rest.sites.basins.nogo$Area_ha>=100))
dim(subset(rest.sites.basins.nogo, rest.sites.basins.nogo$Area_ha<100))

# read in data with BVGs - from intersect in ArcGIS (rest sites and preclear RE, dissolved by FID_tidal and DBVG1M, add field and calculate area)
rest.sites.BVG <- read_excel("rsites4_preclearRE.xls", sheet="rsites4_preclearRE") #update with file name
head(rest.sites.BVG)
sum(rest.sites.BVG$BVG_ha) #restorable area 31686 ha
# select and spread data from long to wide
rest.sites.BVG.tidy <- rest.sites.BVG %>%
  select(FID_site="FID_tidal_", "DBVG1M", "BVG_ha") %>% 
  spread(key = "DBVG1M", value = "BVG_ha", convert=TRUE) # make sure there are no duplicate sites, if so need to dissolve in ArcGIS first
head(rest.sites.BVG.tidy)
#remove CYC sites
rest.sites.BVG.tidy.CYC <- full_join(rest.sites.BVG.tidy, rest.sites.CYC, by="FID_site")
rest.sites.BVG.tidy.nogo <- subset(rest.sites.BVG.tidy.CYC, rest.sites.BVG.tidy.CYC$CYC==0)
dim(rest.sites.BVG.tidy.nogo) #425

# read in data with BVGs and intertidal tidal zones - from intersect of above with intertidal tidal zones
rest.sites.BVG.tid <- read_excel("rsites4_preclearRE_tidal.xls", sheet="rsites4_preclearRE_tidal") #update with file name
head(rest.sites.BVG.tid)
sum(rest.sites.BVG.tid$tidal_ha) #same as above
# select and spread data from long to wide
rest.sites.BVG.tid.tidy <- rest.sites.BVG.tid %>%
  select(FID_site="FID_tidal_", "DBVG1M", Tidal_zone="gridcode", "tidal_ha") %>% 
  spread(key = "DBVG1M", value = "tidal_ha", convert=TRUE) # make sure there are no duplicate sites, if so need to dissolve in ArcGIS first
head(rest.sites.BVG.tid.tidy)
#is there any mangrove in intertidal tidal zone 3 - high?
scrubmang <- subset(rest.sites.BVG.tid.tidy, rest.sites.BVG.tid.tidy$Tidal_zone==3)
sum(na.omit(scrubmang$'35a')) #Yes

# calculate total area habitat types - mangroves, saltmarsh and Melaleuca wetland and add as new columns
table(rest.sites.BVG$DBVG1M) #check which BVGs there are
# Mangroves: 35a 
#	Saltmarsh: 35b
#	Melaleuca brackish: 22a, 22b 
#	Melaleuca fresh: 22c
#	Grassland/sedgeland fresh (palustrine wetlands): 34c, 34d, 34g(none)
# Vine forest: 4a(none), 4b(none)
# Freshwater body: 16d
# Estuary waterbody: estuary
# Woodland fresh: 15b(none), 16a, 16c, 16d
# add total area of each wetland type to dataframe by BVG

mangrove.BVG <- c("35a")
saltmarsh.BVG <- c("35b")
melaleuca.BVG <- c("22a", "22b", "22c") 
sedgeland.BVG <- c("34c", "34d") #no 34g
#vineforest.BVG <- c("4a", "4b") #none
waterholes.BVG <- c("16d")
estuary.BVG <- c("estuary")
wood.BVG <- c("16a", "16c") #include woodland BVGs in rest sites

# note sedgeland has been combined with saltmarsh, and woodland with Melaleuca (supratidal)
# estuary to mangroves, waterholes to supratidal
rest.sites.BVG.tidy[is.na(rest.sites.BVG.tidy)] <- 0
rest.sites.BVG.tidy$mangrove_area_ha <- rest.sites.BVG.tidy$"35a"
rest.sites.BVG.tidy$saltmarsh_area_ha <- rest.sites.BVG.tidy$"35b"
rest.sites.BVG.tidy$melaleuca_area_ha <- rowSums(rest.sites.BVG.tidy[,melaleuca.BVG], na.rm=TRUE)
rest.sites.BVG.tidy$sedgeland_area_ha <- rowSums(rest.sites.BVG.tidy[,sedgeland.BVG], na.rm=TRUE)
#rest.sites.BVG.tidy$vineforest_area_ha <- rowSums(rest.sites.BVG.tidy[,vineforest.BVG], na.rm=TRUE)
rest.sites.BVG.tidy$waterholes_area_ha <- rest.sites.BVG.tidy$"16d"
rest.sites.BVG.tidy$estuary_area_ha <- rest.sites.BVG.tidy$"estuary"
rest.sites.BVG.tidy$wood_area_ha <- rowSums(rest.sites.BVG.tidy[,wood.BVG], na.rm=TRUE)
rest.sites.BVG.tidy$mangrove_ANR_ha <- rest.sites.BVG.tidy$mangrove_area_ha+rest.sites.BVG.tidy$estuary_area_ha
rest.sites.BVG.tidy$saltmarsh_ANR_ha <- rest.sites.BVG.tidy$saltmarsh_area_ha+rest.sites.BVG.tidy$sedgeland_area_ha
rest.sites.BVG.tidy$supratidal_ANR_ha <- rest.sites.BVG.tidy$melaleuca_area_ha+rest.sites.BVG.tidy$waterholes_area_ha+
  rest.sites.BVG.tidy$wood_area_ha
head(rest.sites.BVG.tidy)

# total areas of each habitat type
total<-apply(rest.sites.BVG.tidy, 2, sum, na.rm=T) %>% as.matrix
total1<-total[-1,] %>% as.matrix
colnames(total1)<-"Area_ha"
sum(total1[1:11]) #check
sum(total1[12:18])
sum(total1[19:21])
write.csv(total1, file="rest_areas_BVG.csv", row.names=T)

total.nogo<-apply(rest.sites.BVG.tidy.nogo, 2, sum, na.rm=T) %>% as.matrix
total.nogo.BVG<-total.nogo[-c(1,13:23),] %>% as.matrix
total.nogo.BVG.prop <- prop.table(total.nogo.BVG)
total.nogo.veg<-total.nogo[-c(1:12,20:23),] %>% as.matrix
total.nogo.veg.prop <- prop.table(total.nogo.veg)
#53.6% saltmarsh, 1% mangrove, 1.8% Melaleuca, 1.9% sedgeland and 39.8% floodplain woodlands 


# read in default values from the blue carbon method
abate <- read.csv("Abatement_BlueCAM_parameters.csv", header=T) 
head(abate)
## removal factors
AGB.max.mang <- abate[1,5]
AGB.max.salt <- abate[2,5]
AGB.max.supra <- abate[3,5]
SOC.rate.mang <- abate[7,10]
SOC.rate.salt <- abate[8,10]
SOC.rate.supra <- abate[9,10]
BGB.RS.mang <- abate[10,16]
BGB.RS.salt <- abate[11,16]
BGB.RS.supra <- abate[12,16]
EF.CH4.mang <- abate[13,21]
EF.CH4.salt <- abate[14,21]
EF.CH4.supra <- abate[15,21]
EF.N2O.mang <- abate[16,21]
EF.N2O.salt <- abate[17,21]
EF.N2O.supra <- abate[18,21]

AGB.max.mang.upperCI <- abate[1,8]
AGB.max.mang.lowerCI <- abate[1,9]
AGB.max.salt.upperCI <- abate[2,8]
AGB.max.salt.lowerCI <- abate[2,9]
AGB.max.supra.upperCI <- abate[3,8]
AGB.max.supra.lowerCI <- abate[3,9]
SOC.rate.mang.upperCI <- abate[7,15]
SOC.rate.mang.lowerCI <- abate[7,14] #this is higher than median
SOC.rate.salt.upperCI <- abate[8,15]
SOC.rate.salt.lowerCI <- abate[8,14]
SOC.rate.supra.upperCI <- abate[9,15]
SOC.rate.supra.lowerCI <- abate[9,14]
BGB.RS.mang.upperCI <- abate[10,20]
BGB.RS.mang.lowerCI <- abate[10,19]


# read in global warming potential (GWP) and conversion factors (CF)
GWP <- read.csv("GWP.csv", header=T)
CF <- read.csv("CF.csv", header=T)

GWP.CO2 <- GWP[1,3]
GWP.CH4 <- GWP[2,3]
GWP.N2O <- GWP[3,3]
CF.C.CO2 <- CF[1,3]
CF.N.N2O <- CF[4,3]
CF.C.CH4 <- CF[2,3]


#coastal wetlands CO2e abatement and revenue per restoration site
#removals (AGB and BGB carbon, soil C)
#emissions (CH4 and N2O) 
rest.sites.abate <- rest.sites.BVG.tidy %>% select(FID_site, mangrove_ANR_ha, saltmarsh_ANR_ha, supratidal_ANR_ha)
head(rest.sites.abate)

#constant removals and emissions
#soil C removals
##median
rest.sites.abate$SOC_mang_CO2eMgYr <- rest.sites.abate$mangrove_ANR_ha * SOC.rate.mang * CF.C.CO2
rest.sites.abate$SOC_salt_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * SOC.rate.salt * CF.C.CO2
rest.sites.abate$SOC_supra_CO2eMgYr <- rest.sites.abate$supratidal_ANR_ha * SOC.rate.supra * CF.C.CO2
##upperCI
rest.sites.abate$SOC_mang_upper_CO2eMgYr <- rest.sites.abate$mangrove_ANR_ha * SOC.rate.mang.upperCI * CF.C.CO2
rest.sites.abate$SOC_salt_upper_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * SOC.rate.salt.upperCI * CF.C.CO2
rest.sites.abate$SOC_supra_upper_CO2eMgYr <- rest.sites.abate$supratidal_ANR_ha * SOC.rate.supra.upperCI * CF.C.CO2
##lowerCI
rest.sites.abate$SOC_mang_lower_CO2eMgYr <- rest.sites.abate$mangrove_ANR_ha * SOC.rate.mang.lowerCI * CF.C.CO2
rest.sites.abate$SOC_salt_lower_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * SOC.rate.salt.lowerCI * CF.C.CO2
rest.sites.abate$SOC_supra_lower_CO2eMgYr <- rest.sites.abate$supratidal_ANR_ha * SOC.rate.supra.lowerCI * CF.C.CO2

#CH4 emissions
rest.sites.abate$CH4_mang_CO2eMgYr <- rest.sites.abate$mangrove_ANR_ha * (EF.CH4.mang/1000) * GWP.CH4 
rest.sites.abate$CH4_salt_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * (EF.CH4.salt/1000) * GWP.CH4
rest.sites.abate$CH4_supra_CO2eMgYr <- rest.sites.abate$supratidal_ANR_ha * (EF.CH4.supra/1000) * GWP.CH4

#N2O emissions
rest.sites.abate$N2O_mang_CO2eMgYr <- rest.sites.abate$mangrove_ANR_ha * (EF.N2O.mang/1000) * GWP.N2O
rest.sites.abate$N2O_salt_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * (EF.N2O.salt/1000) * GWP.N2O
rest.sites.abate$N2O_supra_CO2eMgYr <- rest.sites.abate$supratidal_ANR_ha * (EF.N2O.supra/1000) * GWP.N2O

#AGB saltmarsh, attains max at first year
rest.sites.abate$AGB_salt_1yr_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * AGB.max.salt * CF.C.CO2
rest.sites.abate$AGB_salt_1yr_upper_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * AGB.max.salt.upperCI * CF.C.CO2
rest.sites.abate$AGB_salt_1yr_lower_CO2eMgYr <- rest.sites.abate$saltmarsh_ANR_ha * AGB.max.salt.lowerCI * CF.C.CO2

# logistic growth rate for mangrove and supratidal AGB, then BGB ratio applied

# create function with loop
AGBt <- function(a, k, t) {
  a*(exp(-k/t))
}
#a is the maximum above ground biomass and "k" the rate of biomass increase over time
k <- 29.6
ages <- 1:25

#calculate mangrove AGB accumulation for 25 years
mang.AGB.1_25 <- AGBt(a=AGB.max.mang, k=29.6, t = seq(along = ages)) 

#get data into dataframe
# repeat row function
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

### update with number of sites ###
# get mangrove AGB accumulation per year per ha into dataframe for 25 years
length(rest.sites.BVG.tidy$FID_site) #455 sites, so label from 0 to 454 - need to change according to number of sites
mang.AGB.1_25.df <- rep.row(mang.AGB.1_25, 455) %>% as.data.frame
head(mang.AGB.1_25.df)
colnames(mang.AGB.1_25.df) <- paste0("AGB_mang_yr",1:25)
rownames(mang.AGB.1_25.df) <- c(0:454)
dim(mang.AGB.1_25.df)
#convert into accumulation rate per year
mang.AGB.rate.2_25.df <- mang.AGB.1_25.df[2:25] - mang.AGB.1_25.df[1:24]
#append first year
mang.AGB.rate.1_25.df <- cbind(mang.AGB.1_25.df[1], mang.AGB.rate.2_25.df)

# mangrove AGB accumulation (in CO2e) per year per site
AGB_mang.df <- cbind("mangrove_ANR_ha" = rest.sites.abate$mangrove_ANR_ha, mang.AGB.rate.1_25.df)
head(AGB_mang.df)
AGB_mang_growth.df <- AGB_mang.df$mangrove_ANR_ha * AGB_mang.df[2:26]
head(AGB_mang_growth.df)
AGB_mang_growth_CO2eMgYr.df <- AGB_mang_growth.df[1:25] * CF.C.CO2 
head(AGB_mang_growth_CO2eMgYr.df)

##upper
mang.AGB.1_25.upper <- AGBt(a=AGB.max.mang.upperCI, k=29.6, t = seq(along = ages)) 
mang.AGB.1_25.df.upper <- rep.row(mang.AGB.1_25.upper, 455) %>% as.data.frame
colnames(mang.AGB.1_25.df.upper) <- paste0("AGB_mang_yr",1:25)
rownames(mang.AGB.1_25.df.upper) <- c(0:454)
#convert into accumulation rate per year
mang.AGB.rate.2_25.df.upper <- mang.AGB.1_25.df.upper[2:25] - mang.AGB.1_25.df.upper[1:24]
#append first year
mang.AGB.rate.1_25.df.upper <- cbind(mang.AGB.1_25.df.upper[1], mang.AGB.rate.2_25.df.upper)
# mangrove AGB accumulation (in CO2e) per year per site
AGB_mang.df.upper <- cbind("mangrove_ANR_ha" = rest.sites.abate$mangrove_ANR_ha, mang.AGB.rate.1_25.df.upper)
AGB_mang_growth.df.upper <- AGB_mang.df.upper$mangrove_ANR_ha * AGB_mang.df.upper[2:26]
AGB_mang_growth_CO2eMgYr.df.upper <- AGB_mang_growth.df.upper[1:25] * CF.C.CO2 
head(AGB_mang_growth_CO2eMgYr.df.upper)

##lower
mang.AGB.1_25.lower <- AGBt(a=AGB.max.mang.lowerCI, k=29.6, t = seq(along = ages)) 
mang.AGB.1_25.df.lower <- rep.row(mang.AGB.1_25.lower, 455) %>% as.data.frame
colnames(mang.AGB.1_25.df.lower) <- paste0("AGB_mang_yr",1:25)
rownames(mang.AGB.1_25.df.lower) <- c(0:454)
#convert into accumulation rate per year
mang.AGB.rate.2_25.df.lower <- mang.AGB.1_25.df.lower[2:25] - mang.AGB.1_25.df.lower[1:24]
#append first year
mang.AGB.rate.1_25.df.lower <- cbind(mang.AGB.1_25.df.lower[1], mang.AGB.rate.2_25.df.lower)
# mangrove AGB accumulation (in CO2e) per year per site
AGB_mang.df.lower <- cbind("mangrove_ANR_ha" = rest.sites.abate$mangrove_ANR_ha, mang.AGB.rate.1_25.df.lower)
AGB_mang_growth.df.lower <- AGB_mang.df.lower$mangrove_ANR_ha * AGB_mang.df.lower[2:26]
AGB_mang_growth_CO2eMgYr.df.lower <- AGB_mang_growth.df.lower[1:25] * CF.C.CO2 
head(AGB_mang_growth_CO2eMgYr.df.lower)

#calculate supratidal AGB accumulation for 25 years
sup.AGB.1_25 <- AGBt(a=AGB.max.supra, k=29.6, t = seq(along = ages))

# get supratidal AGB accumulation per year per ha into dataframe for 25 years
sup.AGB.1_25.df <- rep.row(sup.AGB.1_25, 455) %>% as.data.frame
head(sup.AGB.1_25.df)
colnames(sup.AGB.1_25.df) <- paste0("AGB_sup_yr",1:25)
rownames(sup.AGB.1_25.df) <- c(0:454)
#convert into accumulation rate per year
sup.AGB.rate.2_25.df <- sup.AGB.1_25.df[2:25] - sup.AGB.1_25.df[1:24]
#append first year
sup.AGB.rate.1_25.df <- cbind(sup.AGB.1_25.df[1], sup.AGB.rate.2_25.df)

# supratidal AGB accumulation (in CO2e) per year per site
AGB_sup.df <- cbind("supratidal_ANR_ha" = rest.sites.abate$supratidal_ANR_ha, sup.AGB.rate.1_25.df)
head(AGB_sup.df)
AGB_sup_growth.df <- (AGB_sup.df$supratidal_ANR_ha) * AGB_sup.df[2:26]
head(AGB_sup_growth.df)
AGB_sup_growth_CO2eMgYr.df <- AGB_sup_growth.df[1:25] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr.df)

##upper
sup.AGB.1_25.upper <- AGBt(a=AGB.max.supra.upperCI, k=29.6, t = seq(along = ages))
sup.AGB.1_25.df.upper <- rep.row(sup.AGB.1_25.upper, 455) %>% as.data.frame
colnames(sup.AGB.1_25.df.upper) <- paste0("AGB_sup_yr",1:25)
rownames(sup.AGB.1_25.df.upper) <- c(0:454)
sup.AGB.rate.2_25.df.upper <- sup.AGB.1_25.df.upper[2:25] - sup.AGB.1_25.df.upper[1:24]
sup.AGB.rate.1_25.df.upper <- cbind(sup.AGB.1_25.df.upper[1], sup.AGB.rate.2_25.df.upper)
AGB_sup.df.upper <- cbind("supratidal_ANR_ha" = rest.sites.abate$supratidal_ANR_ha, sup.AGB.rate.1_25.df.upper)
AGB_sup_growth.df.upper <- (AGB_sup.df.upper$supratidal_ANR_ha) * AGB_sup.df.upper[2:26]
AGB_sup_growth_CO2eMgYr.df.upper <- AGB_sup_growth.df.upper[1:25] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr.df.upper)

##lower
sup.AGB.1_25.lower <- AGBt(a=AGB.max.supra.lowerCI, k=29.6, t = seq(along = ages))
sup.AGB.1_25.df.lower <- rep.row(sup.AGB.1_25.lower, 455) %>% as.data.frame
colnames(sup.AGB.1_25.df.lower) <- paste0("AGB_sup_yr",1:25)
rownames(sup.AGB.1_25.df.lower) <- c(0:454)
sup.AGB.rate.2_25.df.lower <- sup.AGB.1_25.df.lower[2:25] - sup.AGB.1_25.df.lower[1:24]
sup.AGB.rate.1_25.df.lower <- cbind(sup.AGB.1_25.df.lower[1], sup.AGB.rate.2_25.df.lower)
AGB_sup.df.lower <- cbind("supratidal_ANR_ha" = rest.sites.abate$supratidal_ANR_ha, sup.AGB.rate.1_25.df.lower)
AGB_sup_growth.df.lower <- (AGB_sup.df.upper$supratidal_ANR_ha) * AGB_sup.df.lower[2:26]
AGB_sup_growth_CO2eMgYr.df.lower <- AGB_sup_growth.df.lower[1:25] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr.df.lower)

# saltmarsh AGB accumulation (in CO2e) into dataframe for first year per site
AGB_salt_growth_CO2eMgYr.df <- rep.col(rest.sites.abate$AGB_salt_1yr_CO2eMgYr, 25) %>% as.data.frame
AGB_salt_growth_CO2eMgYr.df[2:25] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr.df) <- paste0("AGB_salt_yr",1:25)
head(AGB_salt_growth_CO2eMgYr.df)

##upper
AGB_salt_growth_CO2eMgYr.df.upper <- rep.col(rest.sites.abate$AGB_salt_1yr_upper_CO2eMgYr, 25) %>% as.data.frame
AGB_salt_growth_CO2eMgYr.df.upper[2:25] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr.df.upper) <- paste0("AGB_salt_yr",1:25)
head(AGB_salt_growth_CO2eMgYr.df.upper)

##lower
AGB_salt_growth_CO2eMgYr.df.lower <- rep.col(rest.sites.abate$AGB_salt_1yr_lower_CO2eMgYr, 25) %>% as.data.frame
AGB_salt_growth_CO2eMgYr.df.lower[2:25] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr.df.lower) <- paste0("AGB_salt_yr",1:25)
head(AGB_salt_growth_CO2eMgYr.df.lower)

#apply the BGB ratio - check this applies the function over the entire dataframe
BGB_mang_growth_CO2eMgYr.df <- AGB_mang_growth_CO2eMgYr.df * BGB.RS.mang
BGB_sup_growth_CO2eMgYr.df <- AGB_sup_growth_CO2eMgYr.df * BGB.RS.supra
head(BGB_mang_growth_CO2eMgYr.df)
head(BGB_sup_growth_CO2eMgYr.df) 
#change column names to BGB
colnames(BGB_mang_growth_CO2eMgYr.df) = gsub("AGB", "BGB", colnames(BGB_mang_growth_CO2eMgYr.df))
colnames(BGB_sup_growth_CO2eMgYr.df) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr.df))

##upper
BGB_mang_growth_CO2eMgYr.df.upper <- AGB_mang_growth_CO2eMgYr.df.upper * BGB.RS.mang.upperCI
BGB_sup_growth_CO2eMgYr.df.upper <- AGB_sup_growth_CO2eMgYr.df.upper * BGB.RS.supra #no upper R:S for supra
colnames(BGB_mang_growth_CO2eMgYr.df.upper) = gsub("AGB", "BGB", colnames(BGB_mang_growth_CO2eMgYr.df.upper))
colnames(BGB_sup_growth_CO2eMgYr.df.upper) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr.df.upper))

##lower
BGB_mang_growth_CO2eMgYr.df.lower <- AGB_mang_growth_CO2eMgYr.df.lower * BGB.RS.mang.lowerCI
BGB_sup_growth_CO2eMgYr.df.lower <- AGB_sup_growth_CO2eMgYr.df.lower * BGB.RS.supra #no lower R:S for supra
colnames(BGB_mang_growth_CO2eMgYr.df.lower) = gsub("AGB", "BGB", colnames(BGB_mang_growth_CO2eMgYr.df.lower))
colnames(BGB_sup_growth_CO2eMgYr.df.lower) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr.df.lower))

#100 year dataframe
#calculate mangrove AGB accumulation over 100 years
ages100 <- 1:100
mang.AGB.1_100 <- AGBt(a=AGB.max.mang, k=29.6, t = seq(along = ages100)) 

# get mangrove AGB accumulation per ha into dataframe for 100 years
mang.AGB.1_100.df <- rep.row(mang.AGB.1_100, 455) %>% as.data.frame
head(mang.AGB.1_100.df)
colnames(mang.AGB.1_100.df) <- paste0("AGB_mang_yr",1:100)
rownames(mang.AGB.1_100.df) <- c(0:454)
#convert into accumulation rate per year
mang.AGB.rate.2_100.df <- mang.AGB.1_100.df[2:100] - mang.AGB.1_100.df[1:99]
#append first year
mang.AGB.rate.1_100.df <- cbind(mang.AGB.1_100.df[1], mang.AGB.rate.2_100.df)

# mangrove AGB accumulation (in CO2e) per year per site
AGB_mang_100.df <- cbind("mangrove_ANR_ha" = rest.sites.abate$mangrove_ANR_ha, mang.AGB.rate.1_100.df)
head(AGB_mang_100.df)
AGB_mang_growth_100.df <- AGB_mang_100.df$mangrove_ANR_ha * AGB_mang_100.df[2:101]
head(AGB_mang_growth_100.df)
AGB_mang_growth_100_CO2eMgYr.df <- AGB_mang_growth_100.df[1:100] * CF.C.CO2 
head(AGB_mang_growth_100_CO2eMgYr.df)

#calculate supratidal AGB accumulation over 100 years
sup.AGB.1_100 <- AGBt(a=AGB.max.supra, k=29.6, t = seq(along = ages100))

# get supratidal AGB accumulation per ha into dataframe over 25 years
sup.AGB.1_100.df <- rep.row(sup.AGB.1_100, 455) %>% as.data.frame
head(sup.AGB.1_100.df)
colnames(sup.AGB.1_100.df) <- paste0("AGB_sup_yr",1:100)
rownames(sup.AGB.1_100.df) <- c(0:454)
#convert into accumulation rate per year
sup.AGB.rate.2_100.df <- sup.AGB.1_100.df[2:100] - sup.AGB.1_100.df[1:99]
#append first year
sup.AGB.rate.1_100.df <- cbind(sup.AGB.1_100.df[1], sup.AGB.rate.2_100.df)

# supratidal AGB accumulation (in CO2e) per year per site
AGB_sup_100.df <- cbind("supratidal_ANR_ha" = rest.sites.abate$supratidal_ANR_ha, sup.AGB.rate.1_100.df)
head(AGB_sup_100.df)
AGB_sup_growth_100.df <- (AGB_sup_100.df$supratidal_ANR_ha) * AGB_sup_100.df[2:101]
head(AGB_sup_growth_100.df)
AGB_sup_growth_100_CO2eMgYr.df <- AGB_sup_growth_100.df[1:100] * CF.C.CO2
head(AGB_sup_growth_100_CO2eMgYr.df)

# saltmarsh AGB accumulation (in CO2e) into dataframe for first year per site
AGB_salt_growth_100_CO2eMgYr.df <- rep.col(rest.sites.abate$AGB_salt_1yr_CO2eMgYr, 100) %>% as.data.frame
AGB_salt_growth_100_CO2eMgYr.df[2:100] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_100_CO2eMgYr.df) <- paste0("AGB_salt_yr",1:100)
head(AGB_salt_growth_100_CO2eMgYr.df)

#apply the BGB ratio - check this applies the funtion over the entire dataframe
BGB_mang_growth_100_CO2eMgYr.df <- AGB_mang_growth_100_CO2eMgYr.df * BGB.RS.mang
BGB_sup_growth_100_CO2eMgYr.df <- AGB_sup_growth_100_CO2eMgYr.df * BGB.RS.supra
head(BGB_mang_growth_100_CO2eMgYr.df)
head(BGB_sup_growth_100_CO2eMgYr.df) 
#change column names to BGB
colnames(BGB_mang_growth_100_CO2eMgYr.df) = gsub("AGB", "BGB", colnames(BGB_mang_growth_100_CO2eMgYr.df))
colnames(BGB_sup_growth_100_CO2eMgYr.df) = gsub("AGB", "BGB", colnames(BGB_sup_growth_100_CO2eMgYr.df))

##upper mangrove AGB 100 yr - same code as median, but use 95% upper CI for AGB max
mang.AGB.1_100.upper <- AGBt(a=AGB.max.mang.upperCI, k=29.6, t = seq(along = ages100)) 
mang.AGB.1_100.df.upper <- rep.row(mang.AGB.1_100.upper, 455) %>% as.data.frame
colnames(mang.AGB.1_100.df.upper) <- paste0("AGB_mang_yr",1:100)
rownames(mang.AGB.1_100.df.upper) <- c(0:454)
mang.AGB.rate.2_100.df.upper <- mang.AGB.1_100.df.upper[2:100] - mang.AGB.1_100.df.upper[1:99]
mang.AGB.rate.1_100.df.upper <- cbind(mang.AGB.1_100.df.upper[1], mang.AGB.rate.2_100.df.upper)
AGB_mang_100.df.upper <- cbind("mangrove_ANR_ha" = rest.sites.abate$mangrove_ANR_ha, mang.AGB.rate.1_100.df.upper)
AGB_mang_growth_100.df.upper <- AGB_mang_100.df.upper$mangrove_ANR_ha * AGB_mang_100.df.upper[2:101]
AGB_mang_growth_100_CO2eMgYr.df.upper <- AGB_mang_growth_100.df.upper[1:100] * CF.C.CO2 

##lower mangrove AGB 100 yr - same code as median, but use 95% lower CI for AGB max
mang.AGB.1_100.lower <- AGBt(a=AGB.max.mang.lowerCI, k=29.6, t = seq(along = ages100)) 
mang.AGB.1_100.df.lower <- rep.row(mang.AGB.1_100.lower, 455) %>% as.data.frame
colnames(mang.AGB.1_100.df.lower) <- paste0("AGB_mang_yr",1:100)
rownames(mang.AGB.1_100.df.lower) <- c(0:454)
mang.AGB.rate.2_100.df.lower <- mang.AGB.1_100.df.lower[2:100] - mang.AGB.1_100.df.lower[1:99]
mang.AGB.rate.1_100.df.lower <- cbind(mang.AGB.1_100.df.lower[1], mang.AGB.rate.2_100.df.lower)
AGB_mang_100.df.lower <- cbind("mangrove_ANR_ha" = rest.sites.abate$mangrove_ANR_ha, mang.AGB.rate.1_100.df.lower)
AGB_mang_growth_100.df.lower <- AGB_mang_100.df.lower$mangrove_ANR_ha * AGB_mang_100.df.lower[2:101]
AGB_mang_growth_100_CO2eMgYr.df.lower <- AGB_mang_growth_100.df.lower[1:100] * CF.C.CO2 

##upper supratidal AGB 100 yr - same code as median, but apply 95% upper CI for AGB max
sup.AGB.1_100.upper <- AGBt(a=AGB.max.supra.upperCI, k=29.6, t = seq(along = ages100))
sup.AGB.1_100.df.upper <- rep.row(sup.AGB.1_100.upper, 455) %>% as.data.frame
colnames(sup.AGB.1_100.df.upper) <- paste0("AGB_sup_yr",1:100)
rownames(sup.AGB.1_100.df.upper) <- c(0:454)
sup.AGB.rate.2_100.df.upper <- sup.AGB.1_100.df.upper[2:100] - sup.AGB.1_100.df.upper[1:99]
sup.AGB.rate.1_100.df.upper <- cbind(sup.AGB.1_100.df.upper[1], sup.AGB.rate.2_100.df.upper)
AGB_sup_100.df.upper <- cbind("supratidal_ANR_ha" = rest.sites.abate$supratidal_ANR_ha, sup.AGB.rate.1_100.df.upper)
AGB_sup_growth_100.df.upper <- (AGB_sup_100.df.upper$supratidal_ANR_ha) * AGB_sup_100.df.upper[2:101]
AGB_sup_growth_100_CO2eMgYr.df.upper <- AGB_sup_growth_100.df.upper[1:100] * CF.C.CO2

##lower supratidal AGB 100 yr - same code as median, but apply 95% lower CI for AGB max
sup.AGB.1_100.lower <- AGBt(a=AGB.max.supra.lowerCI, k=29.6, t = seq(along = ages100))
sup.AGB.1_100.df.lower <- rep.row(sup.AGB.1_100.lower, 455) %>% as.data.frame
colnames(sup.AGB.1_100.df.lower) <- paste0("AGB_sup_yr",1:100)
rownames(sup.AGB.1_100.df.lower) <- c(0:454)
sup.AGB.rate.2_100.df.lower <- sup.AGB.1_100.df.lower[2:100] - sup.AGB.1_100.df.lower[1:99]
sup.AGB.rate.1_100.df.lower <- cbind(sup.AGB.1_100.df.lower[1], sup.AGB.rate.2_100.df.lower)
AGB_sup_100.df.lower <- cbind("supratidal_ANR_ha" = rest.sites.abate$supratidal_ANR_ha, sup.AGB.rate.1_100.df.lower)
AGB_sup_growth_100.df.lower <- (AGB_sup_100.df.lower$supratidal_ANR_ha) * AGB_sup_100.df.lower[2:101]
AGB_sup_growth_100_CO2eMgYr.df.lower <- AGB_sup_growth_100.df.lower[1:100] * CF.C.CO2

## upper saltmarsh AGB 100 yr
AGB_salt_growth_100_CO2eMgYr.df.upper <- rep.col(rest.sites.abate$AGB_salt_1yr_upper_CO2eMgYr, 100) %>% as.data.frame
AGB_salt_growth_100_CO2eMgYr.df.upper[2:100] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_100_CO2eMgYr.df.upper) <- paste0("AGB_salt_yr",1:100)

## lower saltmarsh AGB 100 yr
AGB_salt_growth_100_CO2eMgYr.df.lower <- rep.col(rest.sites.abate$AGB_salt_1yr_lower_CO2eMgYr, 100) %>% as.data.frame
AGB_salt_growth_100_CO2eMgYr.df.lower[2:100] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_100_CO2eMgYr.df.lower) <- paste0("AGB_salt_yr",1:100)

##upper BGB - same code as above except apply upper AGB and upper 95% CI for R:S if available (only for mangrove)
BGB_mang_growth_100_CO2eMgYr.df.upper <- AGB_mang_growth_100_CO2eMgYr.df.upper * BGB.RS.mang.upperCI
BGB_sup_growth_100_CO2eMgYr.df.upper <- AGB_sup_growth_100_CO2eMgYr.df.upper * BGB.RS.supra
colnames(BGB_mang_growth_100_CO2eMgYr.df.upper) = gsub("AGB", "BGB", colnames(BGB_mang_growth_100_CO2eMgYr.df.upper))
colnames(BGB_sup_growth_100_CO2eMgYr.df.upper) = gsub("AGB", "BGB", colnames(BGB_sup_growth_100_CO2eMgYr.df.upper))

##lower BGB- same code as above except apply lower AGB and lower 95% CI for R:S if available (only for mangrove)
BGB_mang_growth_100_CO2eMgYr.df.lower <- AGB_mang_growth_100_CO2eMgYr.df.lower * BGB.RS.mang.lowerCI
BGB_sup_growth_100_CO2eMgYr.df.lower <- AGB_sup_growth_100_CO2eMgYr.df.lower * BGB.RS.supra
colnames(BGB_mang_growth_100_CO2eMgYr.df.lower) = gsub("AGB", "BGB", colnames(BGB_mang_growth_100_CO2eMgYr.df.lower))
colnames(BGB_sup_growth_100_CO2eMgYr.df.lower) = gsub("AGB", "BGB", colnames(BGB_sup_growth_100_CO2eMgYr.df.lower))


# Baseline land uses - avoided emissions (grazing)
# read in emission factors from the blue carbon method
EF.CH4.flooded <- abate[21,21]
EF.N2O.flooded <- abate[22,21]
EF.CH4.ponds <- abate[23,21]
SOC.restrict.wetlands <- abate[30,10]
SOC.supra.wetlands <- abate[31,10]
EF.N2O.forestland.Mel <- abate[25,21]
EF.N2O.forestland.unmanaged <- abate[26,21]
EF.N2O.sugarcane <- abate[27,21]
EF.N2O.cropping <- abate[28,21]
EF.N2O.grazing <- abate[29,21]


#select data with different land uses - to identify Fitzroy rsites took grazing (3.2.2, 2.1.0, 4.2.0, 4.2.1), water storage (6.2.2), Defence (1.3.3), marsh/wetland production (6.5.2), and hydromods 
rest.sites.landuse <- read_excel("rsites4_LU.xls", sheet="rsites4_LU")
str(rest.sites.landuse)
rest.sites.landuse.tidy <- rest.sites.landuse %>% 
  select(FID_site="FID_tidal_", "ALUM_code_", "LU_ha")%>% 
  spread(key = "ALUM_code_", value = "LU_ha", convert=TRUE)
head(rest.sites.landuse.tidy) 
dim(rest.sites.landuse.tidy) #455 rows, no double ups
table(rest.sites.landuse$ALUM_code_)
# grazing 2.1.0, 3.2.2 (none), 4.2.0 (none), 4.2.1 (none) and 6.2.2 (dams)
# wetland production 6.5.2
# Defence 1.3.1 (none)

# everything else associated with hydromod wetlands
# other minimal use and residual native cover - 1.30 and 1.3.3 - just slithers
# nature 1.1.3, 1.1.4, 1.1.7 - just slithers
# industry 5.3.0, 5.3.3 
# rural residential / infrastructure 5.4.2, 5.4.3, 5.4.5
# recreation 5.5.3
# mining 5.8.3
# lake 6.1.0
# reservoir, water storage, evap basin 6.2.1 and 6.2.2,  6.2.3
# river 6.3.0
# marsh 6.5.0, 6.5.4

## calculate area of managed grazing per site 3.2.2, 4.2.0, 4.2.1
colnames(rest.sites.landuse.tidy) #no managed grazing

##assume everything is grazing as no Defence land
rest.sites.landuse.tidy$graz_ha <- rowSums(rest.sites.landuse.tidy[2:22], na.rm=TRUE)
rest.sites.landuse.tidy <- rest.sites.landuse.tidy %>% select(FID_site,graz_ha)

# read in ponds and dams data
rest.sites.ponds <- read_excel("rsites4_water_storage.xls", sheet="rsites4_water_storage")
head(rest.sites.ponds)
rest.sites.ponds.tidy <- rest.sites.ponds %>% 
  select(FID_site="FID_tidal_", "FEATURETYP", "pond_ha") %>% 
  spread(key = "FEATURETYP", value="pond_ha", convert=TRUE)
head(rest.sites.ponds.tidy)
colnames(rest.sites.ponds.tidy) <- c("FID_site", "dam_ha", "water_storage_ha")
dim(rest.sites.ponds.tidy) #52 with ponds

# read in hydrologically modified wetlands in the rest sites (tidally-restricted wetlands)
# in ArcGIS: select QLD wetland_areas "HYDROMOD" codes H2M2, H2M2a, H2M2b, H2M2c and H2M2e, H2M3 and H2M5, intersect with rest sites
rest.sites.hydromod <- read_excel("rsites4_hydromod_wetclass.xls", sheet="rsites4_hydromod_wetclass") #need to dissolve each site by hydromod code in ArcGIS to remove duplicate rows per site
table(rest.sites.hydromod$WETCLASS_)
rest.sites.hydro.tidy <- rest.sites.hydromod %>% 
  select(FID_site="FID_tidal_", "WETCLASS_", "wet_ha") %>% 
  spread(key = "WETCLASS_", value = "wet_ha", convert=TRUE)
colnames(rest.sites.hydro.tidy)
apply(rest.sites.hydro.tidy, 2, sum, na.rm=T)

# disturbed wetlands (supratidal forest)
# read in wetland mature regrowth and remnant vegetation in the rest sites
# In ArcGIS: select BVGs 35a, 35b, 22a, 22b and 22c from regrowth layer and remnant layers, union layers, and intersect with rsites
# these are the current disturbed mangrove/saltmarsh and supratidal forest
rest.sites.reg.rem <- read_excel("rsites4_reg_rem_wetland.xls", sheet="rsites4_reg_rem_wetland")
colnames(rest.sites.reg.rem)
dim(rest.sites.reg.rem)
range(rest.sites.reg.rem$regrem_ha)
#subset mangrove/saltmarsh from supratidal
table(rest.sites.reg.rem$DBVG1M)
table(rest.sites.reg.rem$DBVG1M_1)
rsites.regrem.mangsalt <- subset(rest.sites.reg.rem, rest.sites.reg.rem$DBVG1M=="35a" | rest.sites.reg.rem$DBVG1M=="35b" | rest.sites.reg.rem$DBVG1M_1=="35a" | rest.sites.reg.rem$DBVG1M_1=="35b")
dim(rsites.regrem.mangsalt)
range(rsites.regrem.mangsalt$regrem_ha)
rsites.regrem.sup <- subset(rest.sites.reg.rem, rest.sites.reg.rem$DBVG1M=="22a" | rest.sites.reg.rem$DBVG1M=="22b"| rest.sites.reg.rem$DBVG1M=="22c" |
                               rest.sites.reg.rem$DBVG1M_1=="22a" | rest.sites.reg.rem$DBVG1M_1=="22b"| rest.sites.reg.rem$DBVG1M_1=="22c")
dim(rsites.regrem.sup)
range(rsites.regrem.sup$regrem_ha)

#calculate area per site for mangrove/saltmarsh and supratidal
rsites.regrem.mangsalt.area <- with(rsites.regrem.mangsalt, tapply(regrem_ha, FID_tidal_, sum)) %>% as.data.frame
colnames(rsites.regrem.mangsalt.area) <- "regrem_mang_salt_ha" 
rsites.regrem.mangsalt.area <- rsites.regrem.mangsalt.area %>% tibble::rownames_to_column("FID_site") 
str(rsites.regrem.mangsalt.area)
rsites.regrem.mangsalt.area$FID_site <- as.numeric(rsites.regrem.mangsalt.area$FID_site)
range(na.omit(rsites.regrem.mangsalt.area$regrem_mang_salt_ha))

rsites.regrem.sup.area <- with(rsites.regrem.sup, tapply(regrem_ha, FID_tidal_, sum)) %>% as.data.frame
colnames(rsites.regrem.sup.area) <- "regrem_sup_ha" 
rsites.regrem.sup.area <- rsites.regrem.sup.area %>% tibble::rownames_to_column("FID_site") 
str(rsites.regrem.sup.area)
rsites.regrem.sup.area$FID_site <- as.numeric(rsites.regrem.sup.area$FID_site)
range(na.omit(rsites.regrem.sup.area$regrem_sup_ha))

## soil C loss from grazing using SOC baseline map
SOC <- read_excel("rsites4_SOC.xls", sheet = "rsites4_SOC")
head(SOC)
SOC$FID_site <- SOC$FID
SOC2 <- SOC %>% select(FID_site, SUM)

#join data
rest.sites.base <- full_join(rest.sites.landuse.tidy, rest.sites.hydro.tidy, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, rest.sites.ponds.tidy, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, rsites.regrem.mangsalt.area, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, rsites.regrem.sup.area, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, SOC2, by="FID_site")
#rest.sites.base$SOC_t <- rest.sites.base$MEAN*rest.sites.base$Area_ha
dim(rest.sites.base) #check number of rsites

#give zeros to NAs
is.na(rest.sites.base$SUM) #yes
rest.sites.base[is.na(rest.sites.base)] <- 0

# Baseline emissions
# CH4 emissions from flooded agricultural land 
#if using hydromod wetlands, take freshwater hydrologically modified wetlands only
rest.sites.base$flooded_ha <- rest.sites.base$Lacustrine + rest.sites.base$Palustrine + rest.sites.base$Riverine
rest.sites.base$CH4_flooded_CO2eMgYr <- rest.sites.base$flooded_ha * (EF.CH4.flooded/1000) * GWP.CH4 
list(rest.sites.base$CH4_flooded_CO2eMgYr) #check values

# N2O emissions from flooded agricultural land
rest.sites.base$N2O_flooded_CO2eMgYr <- rest.sites.base$flooded_ha * (EF.N2O.flooded/1000) * GWP.N2O 
list(rest.sites.base$N2O_flooded_CO2eMgYr) #check values

# CH4 emissions from ponds
rest.sites.base$CH4_ponds_CO2eMgYr <- (rest.sites.base$dam_ha+rest.sites.base$water_storage_ha) * (EF.CH4.ponds/1000) * GWP.CH4 
list(rest.sites.base$CH4_ponds_CO2eMgYr) #check values

# N2O emissions from forest land - Meleleuca forest - use regrowth and remnant supratidal forest in rest sites
rest.sites.base$N2O_forestland_Mel_CO2eMgYr <- rest.sites.base$regrem_sup_ha * (EF.N2O.forestland.Mel/1000)  * GWP.N2O 
head(rest.sites.base$N2O_forestland_Mel_CO2eMgYr)

# N2O emissions from forest land - unmanaged forest - - use regrowth and remnant mangrove and saltmarsh in rest sites
rest.sites.base$N2O_forestland_unmanaged_CO2eMgYr <- rest.sites.base$regrem_mang_salt_ha * (EF.N2O.forestland.unmanaged/1000)  * GWP.N2O 
head(rest.sites.base$N2O_forestland_unmanaged_CO2eMgYr)

# N2O emissions from managed grazing
#none - no managed grazing land

# soil carbon loss from grazing using Tier 2 approach - soil carbon map and default factors
# equation from IPCC Ch 6 SOCREF * FLU * FMG * FI
# FLU = 1, FMG Improved grassland tropical = 1.17, Moderately degraded grassland tropical = 0.97, Nominally managed (non-degraded) = 1, FI = 1, 
# have used the SOC stock calcuated using zonal statistics for the restoration polygons (SUM) or take MEAN and multiply by restoration area
rest.sites.base$SOC_change_Mg20yr <- rest.sites.base$SUM * 1 * 0.97 * 1
rest.sites.base$SOC_loss_Mg20yr <- rest.sites.base$SUM - rest.sites.base$SOC_change_Mg20yr
rest.sites.base$SOC_loss_CO2eMgYr <- (rest.sites.base$SOC_loss_Mg20yr/20) * CF.C.CO2
head(rest.sites.base$SUM)
head(rest.sites.base$SOC_change_Mg20yr)
head(rest.sites.base$SOC_loss_Mg20yr)

#Baseline removals
# soil carbon removals degraded wetlands - use regrowth and remnant wetland areas in rest sites
#	tidally-restricted wetland (freshwater or brackish)
rest.sites.base$SOC_deg_wet_CO2eMgYr <- rest.sites.base$regrem_mang_salt_ha * SOC.restrict.wetlands * CF.C.CO2  
list(rest.sites.base$SOC_deg_wet_CO2eMgYr) #check values
# supratidal forest
rest.sites.base$SOC_deg_sup_CO2eMgYr <- rest.sites.base$regrem_sup_ha * SOC.supra.wetlands * CF.C.CO2  
list(rest.sites.base$SOC_deg_sup_CO2eMgYr)


# baseline emissions - emissions from flooded land, ponds, forest land, degraded wetlands and soil carbon loss, minus soil carbon accumulation in degraded wetlands
colnames(rest.sites.base)
rest.sites.base$base_net_emissions_CO2eMgYr <- (rest.sites.base$CH4_flooded_CO2eMgYr + rest.sites.base$N2O_flooded_CO2eMgYr + 
                                                  rest.sites.base$CH4_ponds_CO2eMgYr + rest.sites.base$SOC_loss_CO2eMgYr +
                                                  rest.sites.base$N2O_forestland_Mel_CO2eMgYr + rest.sites.base$N2O_forestland_unmanaged_CO2eMgYr) - 
  (rest.sites.base$SOC_deg_wet_CO2eMgYr + rest.sites.base$SOC_deg_sup_CO2eMgYr) 
baseline.emissions.total <- colSums(rest.sites.base[,c(13:15,18:23)], na.rm=T) %>% as.matrix


# total restoration abatement (removals and avoided emissions) given logistic growth

# restoration removals - sum constant variables per year
colnames(rest.sites.abate)
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_SOC_removals_CO2eMgYr = sum(c(SOC_mang_CO2eMgYr, SOC_salt_CO2eMgYr, SOC_supra_CO2eMgYr))) #rowSums(rest.sites.abate[,c(5:7)], na.rm=T)
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_emissions_CO2eMgYr = sum(c(CH4_mang_CO2eMgYr, CH4_salt_CO2eMgYr, CH4_supra_CO2eMgYr,N2O_mang_CO2eMgYr, N2O_salt_CO2eMgYr, N2O_supra_CO2eMgYr))) #rowSums(rest.sites.abate[,c(8:13)], na.rm=T)
#with upper and lower parameters for soil carbon removals
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_SOC_removals_CO2eMgYr_upper = sum(c(SOC_mang_upper_CO2eMgYr, SOC_salt_upper_CO2eMgYr, SOC_supra_upper_CO2eMgYr))) 
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_SOC_removals_CO2eMgYr_lower = sum(c(SOC_mang_lower_CO2eMgYr, SOC_salt_lower_CO2eMgYr, SOC_supra_lower_CO2eMgYr))) 

# calculate carbon abatement per year over 25 years
#create dataframe per year for biomass variables
rest.sites.abate.df<-full_join(rest.sites.base,rest.sites.abate,by="FID_site")
colnames(rest.sites.abate.df)
rest.sites.biomass <- cbind(AGB_mang_growth_CO2eMgYr.df, AGB_sup_growth_CO2eMgYr.df, AGB_salt_growth_CO2eMgYr.df,
                               BGB_mang_growth_CO2eMgYr.df, BGB_sup_growth_CO2eMgYr.df)
colnames(rest.sites.biomass) #check
##with upper CI
rest.sites.biomass.upper <- cbind(AGB_mang_growth_CO2eMgYr.df.upper, AGB_sup_growth_CO2eMgYr.df.upper, AGB_salt_growth_CO2eMgYr.df.upper,
                             BGB_mang_growth_CO2eMgYr.df.upper, BGB_sup_growth_CO2eMgYr.df.upper)
##with lower CI
rest.sites.biomass.lower <- cbind(AGB_mang_growth_CO2eMgYr.df.lower, AGB_sup_growth_CO2eMgYr.df.lower, AGB_salt_growth_CO2eMgYr.df.lower,
                                   BGB_mang_growth_CO2eMgYr.df.lower, BGB_sup_growth_CO2eMgYr.df.lower)

#sum biomass variables
rsites.biomass <- rest.sites.biomass[1:25] + rest.sites.biomass[26:50] + rest.sites.biomass[51:75] +
  rest.sites.biomass[76:100] + rest.sites.biomass[101:125]
dim(rsites.biomass)
colnames(rsites.biomass) <- paste0("Biomass_yr",1:25)
##with upper CI
rsites.biomass.upper <- rest.sites.biomass.upper[1:25] + rest.sites.biomass.upper[26:50] + rest.sites.biomass.upper[51:75] +
  rest.sites.biomass.upper[76:100] + rest.sites.biomass.upper[101:125]
colnames(rsites.biomass.upper) <- paste0("Biomass_yr",1:25)
##with lower CI
rsites.biomass.lower <- rest.sites.biomass.lower[1:25] + rest.sites.biomass.lower[26:50] + rest.sites.biomass.lower[51:75] +
  rest.sites.biomass.lower[76:100] + rest.sites.biomass.lower[101:125]
colnames(rsites.biomass.lower) <- paste0("Biomass_yr",1:25)

#calculate abatement given removals and emissions
rsites.abate <- rsites.biomass[1:25] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate) <- paste0("abate_yr",1:25)
#comparison
rsites.abate[1,c(1:25)]
rsites.biomass[1,c(1:25)]
rest.sites.abate.df$wet_SOC_removals_CO2eMgYr[1]
rest.sites.abate.df$wet_emissions_CO2eMgYr[1]
rest.sites.abate.df$base_net_emissions_CO2eMgYr[1]

##with upper CI
rsites.abate.upper <- rsites.biomass.upper[1:25] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr_upper -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.upper) <- paste0("abate_yr",1:25)
rsites.abate.upper[1,c(1:25)]
##with lower CI
rsites.abate.lower <- rsites.biomass.lower[1:25] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr_lower -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.lower) <- paste0("abate_yr",1:25)
rsites.abate.lower[1,c(1:25)]


# calculate carbon abatement per year over 100 years
#get into dataframe
rsites.biomass.100.df <- cbind(AGB_mang_growth_100_CO2eMgYr.df, AGB_sup_growth_100_CO2eMgYr.df, 
                                AGB_salt_growth_100_CO2eMgYr.df,
                                BGB_mang_growth_100_CO2eMgYr.df, BGB_sup_growth_100_CO2eMgYr.df)
dim(rsites.biomass.100.df)
#sum biomass variables
rsites.biomass.100 <- rsites.biomass.100.df[1:100] + rsites.biomass.100.df[101:200] + rsites.biomass.100.df[201:300] +
  rsites.biomass.100.df[301:400] + rsites.biomass.100.df[401:500]
dim(rsites.biomass.100)
colnames(rsites.biomass.100) <- paste0("Biomass_yr",1:100)
#calculate abatement given removals and emissions
rsites.abate.100 <- rsites.biomass.100[1:100] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.100) <- paste0("abate_yr",1:100)
rsites.abate.100[1,c(1:25)]

##with upper CI
rsites.biomass.100.df.upper <- cbind(AGB_mang_growth_100_CO2eMgYr.df.upper, AGB_sup_growth_100_CO2eMgYr.df.upper, 
                               AGB_salt_growth_100_CO2eMgYr.df.upper,
                               BGB_mang_growth_100_CO2eMgYr.df.upper, BGB_sup_growth_100_CO2eMgYr.df.upper)
rsites.biomass.100.upper <- rsites.biomass.100.df.upper[1:100] + rsites.biomass.100.df.upper[101:200] + rsites.biomass.100.df.upper[201:300] +
  rsites.biomass.100.df.upper[301:400] + rsites.biomass.100.df.upper[401:500]
colnames(rsites.biomass.100.upper) <- paste0("Biomass_yr",1:100)

rsites.abate.100.upper <- rsites.biomass.100.upper[1:100] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr_upper -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.100.upper) <- paste0("abate_yr",1:100)

##with lower CI
rsites.biomass.100.df.lower <- cbind(AGB_mang_growth_100_CO2eMgYr.df.lower, AGB_sup_growth_100_CO2eMgYr.df.lower, 
                                     AGB_salt_growth_100_CO2eMgYr.df.lower,
                                     BGB_mang_growth_100_CO2eMgYr.df.lower, BGB_sup_growth_100_CO2eMgYr.df.lower)
rsites.biomass.100.lower <- rsites.biomass.100.df.lower[1:100] + rsites.biomass.100.df.lower[101:200] + rsites.biomass.100.df.lower[201:300] +
  rsites.biomass.100.df.lower[301:400] + rsites.biomass.100.df.lower[401:500]
colnames(rsites.biomass.100.lower) <- paste0("Biomass_yr",1:100)

rsites.abate.100.lower <- rsites.biomass.100.lower[1:100] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr_lower -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.100.lower) <- paste0("abate_yr",1:100)


#total abatement over 25 years and 100 years
rsites.abate$abate_CO2eMg_25yrs <- rowSums(rsites.abate[1:25], na.rm = T)
rsites.abate.100$abate_CO2eMg_25yrs <- rowSums(rsites.abate.100[1:25], na.rm = T)
rsites.abate.100$abate_CO2eMg_100yrs <- rowSums(rsites.abate.100[1:100], na.rm = T)
sum(rsites.abate$abate_CO2eMg_25yrs)
sum(rsites.abate.100$abate_CO2eMg_25yrs) #same overall outcome when modelled over 25 years and 100 years
sum(rsites.abate.100$abate_CO2eMg_100yrs)

##with upper CI
rsites.abate.upper$abate_CO2eMg_25yrs <- rowSums(rsites.abate.upper[1:25], na.rm = T)
rsites.abate.100.upper$abate_CO2eMg_25yrs_upper <- rowSums(rsites.abate.100.upper[1:25], na.rm = T)
rsites.abate.100.upper$abate_CO2eMg_100yrs_upper <- rowSums(rsites.abate.100.upper[1:100], na.rm = T)
sum(rsites.abate.upper$abate_CO2eMg_25yrs)
sum(rsites.abate.100.upper$abate_CO2eMg_25yrs_upper)
sum(rsites.abate.100.upper$abate_CO2eMg_100yrs_upper)

##with lower CI
rsites.abate.lower$abate_CO2eMg_25yrs <- rowSums(rsites.abate.lower[1:25], na.rm = T)
rsites.abate.100.lower$abate_CO2eMg_25yrs_lower <- rowSums(rsites.abate.100.lower[1:25], na.rm = T)
rsites.abate.100.lower$abate_CO2eMg_100yrs_lower <- rowSums(rsites.abate.100.lower[1:100], na.rm = T)
sum(rsites.abate.lower$abate_CO2eMg_25yrs)
sum(rsites.abate.100.lower$abate_CO2eMg_25yrs_lower)
sum(rsites.abate.100.lower$abate_CO2eMg_100yrs_lower)


#save data required for NPV analysis - carbon abatement per year
#100 year dataframe
rsites.abate.100.2 <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.100)
colnames(rsites.abate.100.2) #check
write.csv(rsites.abate.100.2, file = "rest.sites.abatement.pa.100yrs.csv")

##upper
rsites.abate.100.2.upper <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.100.upper)
colnames(rsites.abate.100.2.upper) #check
write.csv(rsites.abate.100.2.upper, file = "rest.sites.abatement.upperCI.pa.100yrs.csv")

##lower
rsites.abate.100.2.lower <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.100.lower)
colnames(rsites.abate.100.2.lower) #check
write.csv(rsites.abate.100.2.lower, file = "rest.sites.abatement.lowerCI.pa.100yrs.csv")

#25 year dataframe
rsites.abate.2 <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate)
colnames(rsites.abate.2) #check
write.csv(rsites.abate.2, file = "rest.sites.abatement.pa.25yrs.csv")

##upper
rsites.abate.2.upper <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.upper)
colnames(rsites.abate.2.upper) #check
write.csv(rsites.abate.2.upper, file = "rest.sites.abatement.upperCI.pa.25yrs.csv")

##lower
rsites.abate.2.lower <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.lower)
colnames(rsites.abate.2.lower) #check
write.csv(rsites.abate.2.lower, file = "rest.sites.abatement.lowerCI.pa.25yrs.csv")


#totals for each activity to include in manuscript
#but include total AGB and BGB for mangrove, saltmarsh and supratidal so can average per year over 25 years
## calculate abatement for 25 and 100 years from mang, mel and salt

rest.sites.abate.df2 <- cbind(rest.sites.abate.df, rest.sites.biomass, abate_CO2eMg_25yrs=rsites.abate$abate_CO2eMg_25yrs, abate_CO2eMg_100yrs=rsites.abate.100$abate_CO2eMg_100yrs)

rest.sites.abate.df2$AGB_mang_CO2eMg_25yr <- rest.sites.abate.df2 %>%
  select(starts_with("AGB_mang")) %>% 
  rowSums()
rest.sites.abate.df2$AGB_mang_CO2eMgYr <- rest.sites.abate.df2$AGB_mang_CO2eMg_25yr/25

rest.sites.abate.df2$AGB_sup_CO2eMg_25yr <- rest.sites.abate.df2 %>%
  select(starts_with("AGB_sup")) %>% 
  rowSums()
rest.sites.abate.df2$AGB_sup_CO2eMgYr <- rest.sites.abate.df2$AGB_sup_CO2eMg_25yr/25

sum(rest.sites.abate.df2$AGB_salt_1yr_CO2eMgYr) #should be the same as:
sum(rest.sites.abate.df2$AGB_salt_yr1) 
rest.sites.abate.df2$AGB_salt_CO2eMgYr <- rest.sites.abate.df2$AGB_salt_yr1/25 #average over 25years

rest.sites.abate.df2$BGB_mang_CO2eMg_25yr <- rest.sites.abate.df2 %>%
  select(starts_with("BGB_mang")) %>% 
  rowSums()
rest.sites.abate.df2$BGB_mang_CO2eMgYr <- rest.sites.abate.df2$BGB_mang_CO2eMg_25yr/25

rest.sites.abate.df2$BGB_sup_CO2eMg_25yr <- rest.sites.abate.df2 %>%
  select(starts_with("BGB_sup")) %>% 
  rowSums()
rest.sites.abate.df2$BGB_sup_CO2eMgYr <- rest.sites.abate.df2$BGB_sup_CO2eMg_25yr/25

rest.sites.abate.df2$abate_CO2eMg_Yr <- rest.sites.abate.df2$abate_CO2eMg_25yrs/25

# itemise abatement from activities to include in manuscript
colnames(rest.sites.abate.df2)
rest.sites.abate.df2 <- full_join(rest.sites.abate.df2, rest.sites.CYC, by="FID_site")#join Area_ha and sites with CYC to dataframe
rest.sites.abate.sum <- rest.sites.abate.df2 %>% 
  dplyr::select(Area_ha, AGB_mang_CO2eMgYr, AGB_sup_CO2eMgYr, AGB_salt_CO2eMgYr, BGB_mang_CO2eMgYr, BGB_sup_CO2eMgYr,
         CH4_flooded_CO2eMgYr, N2O_flooded_CO2eMgYr, CH4_ponds_CO2eMgYr,  N2O_forestland_Mel_CO2eMgYr, N2O_forestland_unmanaged_CO2eMgYr, SOC_loss_CO2eMgYr, 
         SOC_deg_wet_CO2eMgYr, SOC_deg_sup_CO2eMgYr, 
         base_net_emissions_CO2eMgYr,
         SOC_mang_CO2eMgYr, SOC_salt_CO2eMgYr, SOC_supra_CO2eMgYr, 
         wet_SOC_removals_CO2eMgYr,
         CH4_mang_CO2eMgYr, CH4_salt_CO2eMgYr, CH4_supra_CO2eMgYr, 
         N2O_mang_CO2eMgYr, N2O_salt_CO2eMgYr, N2O_supra_CO2eMgYr,
         wet_emissions_CO2eMgYr,
         abate_CO2eMg_25yrs, abate_CO2eMg_100yrs, abate_CO2eMg_Yr)
rest.sites.abate.sum <- apply(rest.sites.abate.sum,2, sum, na.rm=T) %>% as.matrix
write.csv(rest.sites.abate.sum, file = "rest.sites.abate.activites.totals.csv")

#do without nogo sites
rest.sites.abate.nogo <- subset(rest.sites.abate.df2, rest.sites.abate.df2$CYC==0)
rest.sites.abate.nogo.sum <- rest.sites.abate.nogo %>% 
  dplyr::select(Area_ha, AGB_mang_CO2eMgYr, AGB_sup_CO2eMgYr, AGB_salt_CO2eMgYr, BGB_mang_CO2eMgYr, BGB_sup_CO2eMgYr,
                CH4_flooded_CO2eMgYr, N2O_flooded_CO2eMgYr, CH4_ponds_CO2eMgYr,N2O_forestland_Mel_CO2eMgYr, N2O_forestland_unmanaged_CO2eMgYr, SOC_loss_CO2eMgYr, 
                SOC_deg_wet_CO2eMgYr, SOC_deg_sup_CO2eMgYr, 
                base_net_emissions_CO2eMgYr,
                SOC_mang_CO2eMgYr, SOC_salt_CO2eMgYr, SOC_supra_CO2eMgYr, 
                wet_SOC_removals_CO2eMgYr,
                CH4_mang_CO2eMgYr, CH4_salt_CO2eMgYr, CH4_supra_CO2eMgYr, 
                N2O_mang_CO2eMgYr, N2O_salt_CO2eMgYr, N2O_supra_CO2eMgYr,
                wet_emissions_CO2eMgYr,
                abate_CO2eMg_25yrs, abate_CO2eMg_100yrs,abate_CO2eMg_Yr)
rest.sites.abate.nogo.sum2 <- apply(rest.sites.abate.nogo.sum,2, sum, na.rm=T) %>% as.matrix
write.csv(rest.sites.abate.nogo.sum2, file = "rest.sites.abate.activites.totals.without.nogo.csv")
