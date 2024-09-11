# Reads in and manipulates restoration areas and calculates blue carbon - avoided emissions and removals
# Valerie Hagger 15/02/2024
rm(list = ls())
setwd("R:/NESP115-Q4418/R_working_directory/Peel") #sets the working directory

# load packages
library(tidyverse)
library(readxl)

### S1 Removals and Avoided Emissions ###

# agricultural land uses in HAT
#Land use area in HAT
#LUs in HAT - do intersect in ArcGIS of tide5dem_PeelHarveySW  and WA_grazing, dissolve by Secondary_v8 and add field (alum_ha) 
options("scipen" = 100, "digits" = 5) # suppress math annotation
HAT.lu <- read_csv("tidal_grazing_peelharveySW_Dis.csv")
colnames(HAT.lu)
#[1] "OID_"         "TERTIARY_V8"  "SECONDARY_V8"
#[4] "PRIMARY_V8"   "Shape_Length" "Shape_Area"  
#[7] "Landuse_ha"

table(HAT.lu$SECONDARY_V8)
#3.2 Grazing modified pastures 1 
#4.2 Grazing irrigated modified pastures 1
HAT.ag <- subset(HAT.lu, HAT.lu$SECONDARY_V8=="3.2 Grazing modified pastures" |
                           HAT.lu$SECONDARY_V8=="4.2 Grazing irrigated modified pastures")
HAT.lu.total <- sum(HAT.lu$Landuse_ha)
#443.456
HAT.ag.total <- sum(HAT.ag$Landuse_ha)
#443.456

HAT.ag.sec<-with(HAT.ag, tapply(Landuse_ha, SECONDARY_V8, sum)) %>% as.matrix


#Land use area in rest sites
rest.sites.lu <- read_csv("PHSW_grazrsites_landuse_Diss.csv")
colnames(rest.sites.lu)
#[1] "OID_"                               
#[2] "FID_PeelHarveySW_rsites_grazing_1ha"
#[3] "restor_ha"                          
#[4] "SECONDARY_V8"                       
#[5] "Shape_Length"                       
#[6] "Shape_Area"                         
#[7] "alum_ha"
rest.sites.lu.sec <- with(rest.sites.lu, tapply(alum_ha, SECONDARY_V8, sum))
rest.sites.grazing <- sum(rest.sites.lu.sec[1:2])
#348.224

## what percentage of restoration area is ag land within HAT
sum(rest.sites.lu.sec)/sum(HAT.ag.total) 
#0.78525%

HAT.ag.prop <- prop.table(HAT.ag.sec, margin=2)
colnames(HAT.ag.prop) <-"HAT.prop"
ag.HAT.all <- cbind(HAT.ag.sec, HAT.ag.prop) %>% as.matrix


#preclear REs in HAT - do intersect in ArcGIS of tid5dem_fitzroy and preclearRE, dissolve by veg_type and add field (veg_ha) 
HAT.preRE <- read_csv("tide5dem_preclearwetland_Diss.csv")
colnames(HAT.preRE)
#[1] "OID_"         "veg_type"     "Shape_Length"
#[4] "Shape_Area"   "veg_ha"
table(HAT.preRE$veg_type)
#3   6   9  10  14  51 101 107
HAT.preveg.sum<-with(HAT.preRE, tapply(veg_ha, veg_type, sum)) %>% as.data.frame

# Coastal wetland restoration - removals
# read in data with BVGs - do intersect in ArcGIS (rest sites and preclear RE, dissolve by tidal and veg_type, add field PCveg_ha and calculate area)
rest.sites.BVG <- read_csv("PHSWgrazrsites_preclearveg_Dis.csv") #update with file name
head(rest.sites.BVG)
colnames(rest.sites.BVG)
#[1] "OID_"                               
#[2] "FID_PeelHarveySW_rsites_grazing_1ha"
#[3] "restor_ha"                          
#[4] "veg_type"                           
#[5] "Shape_Length"                       
#[6] "Shape_Area"                         
#[7] "PCveg_ha" 

sum(rest.sites.BVG$PCveg_ha) 
#restorable area 348.22ha
# select and spread data from long to wide
rest.sites.BVG.tidy <- rest.sites.BVG %>%
  dplyr::select(FID_site="FID_PeelHarveySW_rsites_grazing_1ha", "veg_type", "PCveg_ha") %>% 
  spread(key = "veg_type", value = "PCveg_ha", convert=TRUE) # make sure there are no duplicate sites, if so need to dissolve in ArcGIS first
head(rest.sites.BVG.tidy)

# calculate total area habitat types - mangroves, saltmarsh and supratidal wetland and add as new columns
table(rest.sites.BVG$veg_type) #check which BVGs there are
#Mangrove: NA
#Saltmarsh: 51
#Supratidal: 3, 6, 9, 14, 101

# add total area of each wetland type to dataframe by BVG

saltmarsh.BVG <- c("51")
supratidal.BVG <- c("3", "6", "9", "14", "101") 

# note sedgeland has been combined with saltmarsh, and vine forest and woodland with Melaleuca (supratidal)
# estuary to mangroves, waterholes to supratidal
rest.sites.BVG.tidy[is.na(rest.sites.BVG.tidy)] <- 0
rest.sites.BVG.tidy$saltmarsh_area_ha <- rest.sites.BVG.tidy$"51"
rest.sites.BVG.tidy$supratidal_area_ha <- rowSums(rest.sites.BVG.tidy[,supratidal.BVG], na.rm=TRUE)

head(rest.sites.BVG.tidy)

#site sizes
dim(subset(rest.sites.BVG, rest.sites.BVG$restor_ha<30))
dim(subset(rest.sites.BVG, rest.sites.BVG$restor_ha>=30))
range(rest.sites.BVG$restor_ha)
sum(rest.sites.BVG$restor_ha)

# total areas of each habitat type
total<-apply(rest.sites.BVG.tidy, 2, sum, na.rm=T) %>% as.matrix
total1<-total[-1,] %>% as.matrix
colnames(total1)<-"Area_ha"
write.csv(total1, file="rest_areas_BVG.csv", row.names=T)

# read in default values from the blue carbon method
abate <- read.csv("Abatement_BlueCAM_parameters.csv", header=T) #spreadsheet with the values from the blue carbon method
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

AGB.max.mang.temp <- abate[4,5]
AGB.max.salt.temp <- abate[5,5]
AGB.max.supra.temp <- abate[6,5]
AGB.max.mang.temp.upperCI <- abate[4,8]
AGB.max.mang.temp.lowerCI <- abate[4,9]
AGB.max.salt.temp.upperCI <- abate[5,8]
AGB.max.salt.temp.lowerCI <- abate[5,9]
AGB.max.supra.temp.upperCI <- abate[6,8]
AGB.max.supra.temp.lowerCI <- abate[6,9]

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
rest.sites.abate <- rest.sites.BVG.tidy %>% dplyr::select(FID_site, saltmarsh_area_ha, supratidal_area_ha)
head(rest.sites.abate)

#constant removals and emissions
#soil C removals
##median
rest.sites.abate$SOC_salt_CO2eMgYr <- rest.sites.abate$saltmarsh_area_ha * SOC.rate.salt * CF.C.CO2
rest.sites.abate$SOC_supra_CO2eMgYr <- rest.sites.abate$supratidal_area_ha * SOC.rate.supra * CF.C.CO2
##upper
rest.sites.abate$SOC_salt_CO2eMgYr_upper <- rest.sites.abate$saltmarsh_area_ha * SOC.rate.salt.upperCI * CF.C.CO2
rest.sites.abate$SOC_supra_CO2eMgYr_upper <- rest.sites.abate$supratidal_area_ha * SOC.rate.supra.upperCI * CF.C.CO2
##lower
rest.sites.abate$SOC_salt_CO2eMgYr_lower <- rest.sites.abate$saltmarsh_area_ha * SOC.rate.salt.lowerCI * CF.C.CO2
rest.sites.abate$SOC_supra_CO2eMgYr_lower <- rest.sites.abate$supratidal_area_ha * SOC.rate.supra.lowerCI * CF.C.CO2

#CH4 emissions
##median
rest.sites.abate$CH4_salt_CO2eMgYr <- rest.sites.abate$saltmarsh_area_ha * (EF.CH4.salt/1000) * GWP.CH4
rest.sites.abate$CH4_supra_CO2eMgYr <- rest.sites.abate$supratidal_area_ha * (EF.CH4.supra/1000) * GWP.CH4

#N2O emissions
rest.sites.abate$N2O_salt_CO2eMgYr <- rest.sites.abate$saltmarsh_area_ha * (EF.N2O.salt/1000) * GWP.N2O
rest.sites.abate$N2O_supra_CO2eMgYr <- rest.sites.abate$supratidal_area_ha * (EF.N2O.supra/1000) * GWP.N2O

#AGB saltmarsh, attains max at first year
##median
rest.sites.abate$AGB_salt_1yr_CO2eMgYr <- rest.sites.abate$saltmarsh_area_ha * AGB.max.salt.temp * CF.C.CO2
dim(rest.sites.abate)
#43 10
rest.sites.abate$AGB_salt_1yr_CO2eMgYr #only a few sites have saltmarsh
##upper
rest.sites.abate$AGB_salt_1yr_CO2eMgYr_upper <- rest.sites.abate$saltmarsh_area_ha * AGB.max.salt.temp.upperCI * CF.C.CO2
##lower
rest.sites.abate$AGB_salt_1yr_CO2eMgYr_lower <- rest.sites.abate$saltmarsh_area_ha * AGB.max.salt.temp.lowerCI * CF.C.CO2

# logistic growth rate for supratidal AGB, then BGB ratio applied
# create function with loop
AGBt <- function(a, k, t) {
  a*(exp(-k/t))
}
#a is the maximum above ground biomass and "k" the rate of biomass increase over time
k <- 29.6
ages <- 1:25

#get data into dataframe
# repeat row function
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# 25 year dataframe

# supratidal AGB accumulation per year per ha into dataframe for 25 years
sup.AGB.1_25 <- AGBt(a=AGB.max.supra.temp, k=29.6, t = seq(along = ages))
length(rest.sites.BVG.tidy$FID_site) #43 sites, so label from 0 to 42 - need to change according to number of sites
sup.AGB.1_25.df <- rep.row(sup.AGB.1_25, 43) %>% as.data.frame
head(sup.AGB.1_25.df)
colnames(sup.AGB.1_25.df) <- paste0("AGB_sup_yr",1:25)
rownames(sup.AGB.1_25.df) <- c(0:42) ### update with number of sites ###
#convert into accumulation rate per year
sup.AGB.rate.2_25.df <- sup.AGB.1_25.df[2:25] - sup.AGB.1_25.df[1:24]
#append first year
sup.AGB.rate.1_25.df <- cbind(sup.AGB.1_25.df[1], sup.AGB.rate.2_25.df)

# supratidal AGB accumulation (in CO2e) per year per site
AGB_sup_25.df <- cbind("supratidal_area_ha" = rest.sites.abate$supratidal_area_ha, sup.AGB.rate.1_25.df)
head(AGB_sup_25.df)
AGB_sup_growth_25.df <- (AGB_sup_25.df$supratidal_area_ha) * AGB_sup_25.df[2:26]
head(AGB_sup_growth_25.df)
AGB_sup_growth_CO2eMgYr_25.df <- AGB_sup_growth_25.df[1:25] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr_25.df)

##upper
# supratidal AGB accumulation per year per ha into dataframe for 25 years
sup.AGB.1_25.upper <- AGBt(a=AGB.max.supra.temp.upperCI, k=29.6, t = seq(along = ages))
sup.AGB.1_25.df.upper <- rep.row(sup.AGB.1_25.upper, 43) %>% as.data.frame
colnames(sup.AGB.1_25.df.upper) <- paste0("AGB_sup_yr",1:25)
rownames(sup.AGB.1_25.df.upper) <- c(0:42) ### update with number of sites ###
#convert into accumulation rate per year
sup.AGB.rate.2_25.df.upper <- sup.AGB.1_25.df.upper[2:25] - sup.AGB.1_25.df.upper[1:24]
#append first year
sup.AGB.rate.1_25.df.upper <- cbind(sup.AGB.1_25.df.upper[1], sup.AGB.rate.2_25.df.upper)
# supratidal AGB accumulation (in CO2e) per year per site
AGB_sup_25.df.upper <- cbind("supratidal_area_ha" = rest.sites.abate$supratidal_area_ha, sup.AGB.rate.1_25.df.upper)
AGB_sup_growth_25.df.upper <- (AGB_sup_25.df.upper$supratidal_area_ha) * AGB_sup_25.df.upper[2:26]
AGB_sup_growth_CO2eMgYr_25.df.upper <- AGB_sup_growth_25.df.upper[1:25] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr_25.df.upper)

##lower
# supratidal AGB accumulation per year per ha into dataframe for 25 years
sup.AGB.1_25.lower <- AGBt(a=AGB.max.supra.temp.lowerCI, k=29.6, t = seq(along = ages))
sup.AGB.1_25.df.lower <- rep.row(sup.AGB.1_25.lower, 43) %>% as.data.frame
colnames(sup.AGB.1_25.df.lower) <- paste0("AGB_sup_yr",1:25)
rownames(sup.AGB.1_25.df.lower) <- c(0:42) ### update with number of sites ###
#convert into accumulation rate per year
sup.AGB.rate.2_25.df.lower <- sup.AGB.1_25.df.lower[2:25] - sup.AGB.1_25.df.lower[1:24]
#append first year
sup.AGB.rate.1_25.df.lower <- cbind(sup.AGB.1_25.df.lower[1], sup.AGB.rate.2_25.df.lower)
# supratidal AGB accumulation (in CO2e) per year per site
AGB_sup_25.df.lower <- cbind("supratidal_area_ha" = rest.sites.abate$supratidal_area_ha, sup.AGB.rate.1_25.df.lower)
AGB_sup_growth_25.df.lower <- (AGB_sup_25.df.lower$supratidal_area_ha) * AGB_sup_25.df.lower[2:26]
AGB_sup_growth_CO2eMgYr_25.df.lower <- AGB_sup_growth_25.df.lower[1:25] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr_25.df.lower)

# saltmarsh AGB accumulation (in CO2e) into dataframe for first year per site
AGB_salt_growth_CO2eMgYr_25.df <- rep.col(rest.sites.abate$AGB_salt_1yr_CO2eMgYr, 25) %>% as.data.frame
AGB_salt_growth_CO2eMgYr_25.df[2:25] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr_25.df) <- paste0("AGB_salt_yr",1:25)
head(AGB_salt_growth_CO2eMgYr_25.df)
##upper
AGB_salt_growth_CO2eMgYr_25.df.upper <- rep.col(rest.sites.abate$AGB_salt_1yr_CO2eMgYr_upper, 25) %>% as.data.frame
AGB_salt_growth_CO2eMgYr_25.df.upper[2:25] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr_25.df.upper) <- paste0("AGB_salt_yr",1:25)
head(AGB_salt_growth_CO2eMgYr_25.df.upper)
##lower
AGB_salt_growth_CO2eMgYr_25.df.lower <- rep.col(rest.sites.abate$AGB_salt_1yr_CO2eMgYr_lower, 25) %>% as.data.frame
AGB_salt_growth_CO2eMgYr_25.df.lower[2:25] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr_25.df.lower) <- paste0("AGB_salt_yr",1:25)
head(AGB_salt_growth_CO2eMgYr_25.df.lower)

#apply the BGB ratio - check this applies the funtion over the entire dataframe
BGB_sup_growth_CO2eMgYr_25.df <- AGB_sup_growth_CO2eMgYr_25.df * BGB.RS.supra
BGB_sup_growth_CO2eMgYr_25.df[1:5,1:5] 
#change column names to BGB
colnames(BGB_sup_growth_CO2eMgYr_25.df) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr_25.df))
##upper
BGB_sup_growth_CO2eMgYr_25.df.upper <- AGB_sup_growth_CO2eMgYr_25.df.upper * BGB.RS.supra
colnames(BGB_sup_growth_CO2eMgYr_25.df.upper) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr_25.df.upper))
##lower
BGB_sup_growth_CO2eMgYr_25.df.lower <- AGB_sup_growth_CO2eMgYr_25.df.lower * BGB.RS.supra
colnames(BGB_sup_growth_CO2eMgYr_25.df.lower) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr_25.df.lower))

# 100 year dataframe
ages100 <- 1:100
sup.AGB.1_100 <- AGBt(a=AGB.max.supra.temp, k=29.6, t = seq(along = ages100))

# get supratidal AGB accumulation per year per ha into dataframe for 100 years
sup.AGB.1_100.df <- rep.row(sup.AGB.1_100, 43) %>% as.data.frame
head(sup.AGB.1_100.df)
colnames(sup.AGB.1_100.df) <- paste0("AGB_sup_yr",1:100)
rownames(sup.AGB.1_100.df) <- c(0:42) ### update with number of sites ###
#convert into accumulation rate per year
sup.AGB.rate.2_100.df <- sup.AGB.1_100.df[2:100] - sup.AGB.1_100.df[1:99]
#append first year
sup.AGB.rate.1_100.df <- cbind(sup.AGB.1_100.df[1], sup.AGB.rate.2_100.df)

# supratidal AGB accumulation (in CO2e) per year per site
AGB_sup_100.df <- cbind("supratidal_area_ha" = rest.sites.abate$supratidal_area_ha, sup.AGB.rate.1_100.df)
head(AGB_sup_100.df)
AGB_sup_growth_100.df <- (AGB_sup_100.df$supratidal_area_ha) * AGB_sup_100.df[2:101]
head(AGB_sup_growth_100.df)
AGB_sup_growth_CO2eMgYr_100.df <- AGB_sup_growth_100.df[1:100] * CF.C.CO2
head(AGB_sup_growth_CO2eMgYr_100.df)

# saltmarsh AGB accumulation (in CO2e) into dataframe for first year per site
AGB_salt_growth_CO2eMgYr_100.df <- rep.col(rest.sites.abate$AGB_salt_1yr_CO2eMgYr, 100) %>% as.data.frame
AGB_salt_growth_CO2eMgYr_100.df[2:100] <-0 #replace all values from year 2 with 0
colnames(AGB_salt_growth_CO2eMgYr_100.df) <- paste0("AGB_salt_yr",1:100)
AGB_salt_growth_CO2eMgYr_100.df[16:20,1:5]

#apply the BGB ratio - check this applies the funtion over the entire dataframe
BGB_sup_growth_CO2eMgYr_100.df <- AGB_sup_growth_CO2eMgYr_100.df * BGB.RS.supra
BGB_sup_growth_CO2eMgYr_100.df[1:5,1:5] 
#change column names to BGB
colnames(BGB_sup_growth_CO2eMgYr_100.df) = gsub("AGB", "BGB", colnames(BGB_sup_growth_CO2eMgYr_100.df))

##upper?
##lower?

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

#select data - don't need this step for Peel cause all grazing land
rest.sites.landuse <- read_csv("PHSW_grazrsites_landuse_Diss.csv")
str(rest.sites.landuse)
colnames(rest.sites.landuse)
#[1] "OID_"                               
#[2] "FID_PeelHarveySW_rsites_grazing_1ha"
#[3] "restor_ha"                          
#[4] "SECONDARY_V8"                       
#[5] "Shape_Length"                       
#[6] "Shape_Area"                         
#[7] "alum_ha" 
rest.sites.landuse.tidy <- rest.sites.landuse %>% 
  dplyr::select(FID_site="FID_PeelHarveySW_rsites_grazing_1ha", "SECONDARY_V8", "alum_ha")%>% 
  spread(key = "SECONDARY_V8", value = "alum_ha", convert=TRUE)
head(rest.sites.landuse.tidy) 
dim(rest.sites.landuse.tidy)
#43 3
table(rest.sites.landuse$SECONDARY_V8)
#3.2 Grazing modified pastures 42 
#4.2 Grazing irrigated modified pastures 1

#so assume everything is grazing
rest.sites.landuse.tidy$graz_ha <- rowSums(rest.sites.landuse.tidy[2:3], na.rm=TRUE) #columns 2 and 3 are the two types of grazing
rest.sites.landuse.tidy <- rest.sites.landuse.tidy %>% dplyr::select(FID_site,graz_ha)

# read in ponds and dams data
rest.sites.ponds <- read_csv("PHSW_rsites_farmdams_Dis.csv")
head(rest.sites.ponds)
colnames(rest.sites.ponds)
#[1] "OID_"                               
#[2] "fid_1"                              
#[3] "FID_PeelHarveySW_rsites_grazing_1ha"
#[4] "restor_ha"                          
#[5] "Shape_Length"                       
#[6] "Shape_Area"                         
#[7] "dam_ha"
#[8] "dam_type" 

rest.sites.ponds.tidy <- rest.sites.ponds %>% 
  dplyr::select(FID_site="FID_PeelHarveySW_rsites_grazing_1ha", "dam_type", "dam_ha") %>% 
  spread(key = "dam_type", value="dam_ha", convert=TRUE)
head(rest.sites.ponds.tidy)
colnames(rest.sites.ponds.tidy) <- c("FID_site", "dam_ha")
dim(rest.sites.ponds.tidy) #52 with ponds
#4 sites

# disturbed wetlands
# read in remnant vegetation in the rest sites identified in ArcGIS
rest.sites.reg.rem <- read_csv("PHSWgrazrsites_remnant_Dis.csv")
colnames(rest.sites.reg.rem)
#[1] "OID_"                               
#[2] "FID_PeelHarveySW_rsites_grazing_1ha"
#[3] "comments"                           
#[4] "Shape_Length"                       
#[5] "Shape_Area"                         
#[6] "rem_ha"  
rsites.reg.rem.area <- with(rest.sites.reg.rem, tapply(rem_ha, FID_PeelHarveySW_rsites_grazing_1ha, sum)) %>% as.data.frame
colnames(rsites.reg.rem.area) <- "regrem_ha" 
rsites.reg.rem.area <- rsites.reg.rem.area %>% tibble::rownames_to_column("FID_site") 
str(rsites.reg.rem.area)
rsites.reg.rem.area$FID_site <- as.numeric(rsites.reg.rem.area$FID_site)
colnames(rsites.reg.rem.area)

## soil C loss from grazing using SOC baseline map
SOC <- read_csv("SOCavPeel_rsites_grazing_zonal.csv")
head(SOC)
SOC$FID_site <- SOC$FID
SOC2 <- SOC %>% dplyr::select(FID_site, SUM)

#join data
rest.sites.base <- full_join(rest.sites.landuse.tidy, rest.sites.BVG.tidy, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, rest.sites.ponds.tidy, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, rsites.reg.rem.area, by="FID_site")
rest.sites.base <- full_join(rest.sites.base, SOC2, by="FID_site")
#rest.sites.base$SOC_t <- rest.sites.base$MEAN*rest.sites.base$Area_ha
dim(rest.sites.base) #check number of rsites

#give zeros to NAs
is.na(rest.sites.base$SUM) #yes
rest.sites.base[is.na(rest.sites.base)] <- 0

# Baseline emissions
# No flooded agricultural land mapped
# CH4 emissions from ponds
rest.sites.base$CH4_ponds_CO2eMgYr <- (rest.sites.base$dam_ha) * (EF.CH4.ponds/1000) * GWP.CH4 
list(rest.sites.base$CH4_ponds_CO2eMgYr) #check values

# N2O emissions from forest land - Meleleuca forest - use remnant veg in rest sites
rest.sites.base$N2O_forestland_Mel_CO2eMgYr <- rest.sites.base$regrem_ha * (EF.N2O.forestland.Mel/1000)  * GWP.N2O 
head(rest.sites.base$N2O_forestland_Mel_CO2eMgYr)

# N2O emissions from forest land - unmanaged forest - don't have data to identify this, put all existing remnant veg in the fores land Mel forest category
#rest.sites.base$N2O_forestland_unmanaged_CO2eMgYr <- rest.sites.base$regrem_ha * (EF.N2O.forestland.unmanaged/1000)  * GWP.N2O 
#head(rest.sites.base$N2O_forestland_unmanaged_CO2eMgYr)

# N2O emissions from managed grazing
# mapped as 3.2 Grazing modified pastures and #4.2 Grazing irrigated modified pastures
# assume this is managed grazing land
rest.sites.base$N2O_grazing_CO2eMgYr <- rest.sites.base$graz_ha * (EF.N2O.grazing/1000)  * GWP.N2O 

# soil C loss from grazing using Tier 2 approach - soil carbon map and default factors
# equation from IPCC Ch 6 SOCREF * FLU * FMG * FI
# FLU = 1, FMG Improved grassland tropical = 1.17, Moderately degraded grassland tropical = 0.97, Nominally managed (non-degraded) = 1, FI = 1, 
# have used the SOC stock calcuated using zonal statistics for the restoration polygons (SUM) or take MEAN and multiply by restoration area
rest.sites.base$SOC_change_Mg20yr <- rest.sites.base$SUM * 1 * 0.97 * 1
rest.sites.base$SOC_loss_Mg20yr <- rest.sites.base$SUM - rest.sites.base$SOC_change_Mg20yr
rest.sites.base$SOC_loss_CO2eMgYr <- (rest.sites.base$SOC_loss_Mg20yr/20) * CF.C.CO2
head(rest.sites.base$SUM)
head(rest.sites.base$SOC_change_Mg20yr)
head(rest.sites.base$SOC_loss_Mg20yr)

# Baseline removals
# soil C removals degraded wetlands - use remnant wetland areas in rest sites
rest.sites.base$SOC_deg_wet_CO2eMgYr <- rest.sites.base$regrem_ha * SOC.supra.wetlands * CF.C.CO2  
list(rest.sites.base$SOC_deg_wet_CO2eMgYr) #check values

# Baseline net emissions - emissions from ponds, forest land, managed grazing and SOC loss, minus SOC accumulation in degraded wetlands
colnames(rest.sites.base)
rest.sites.base$base_net_emissions_CO2eMgYr <- (rest.sites.base$CH4_ponds_CO2eMgYr + rest.sites.base$SOC_loss_CO2eMgYr +
                                                  rest.sites.base$N2O_grazing_CO2eMgYr + rest.sites.base$N2O_forestland_Mel_CO2eMgYr) - 
  rest.sites.base$SOC_deg_wet_CO2eMgYr #(rowSums(rest.sites.base[,c(33,34,35,39)], na.rm=T))
baseline.emissions.total <- colSums(rest.sites.base[,c(14:17,20:21)], na.rm=T) %>% as.matrix


# total restoration abatement (removals and avoided emissions) given logistic growth
# restoration removals - sum constant ones per year
colnames(rest.sites.abate)
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_SOC_removals_CO2eMgYr = sum(c(SOC_salt_CO2eMgYr, SOC_supra_CO2eMgYr))) #rowSums(rest.sites.abate[,c(5:7)], na.rm=T)
##upper
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_SOC_removals_CO2eMgYr_upper = sum(c(SOC_salt_CO2eMgYr_upper, SOC_supra_CO2eMgYr_upper))) #rowSums(rest.sites.abate[,c(5:7)], na.rm=T)
##lower
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_SOC_removals_CO2eMgYr_lower = sum(c(SOC_salt_CO2eMgYr_lower, SOC_supra_CO2eMgYr_lower))) #rowSums(rest.sites.abate[,c(5:7)], na.rm=T)

# restoration emissions - sum constant ones per year
rest.sites.abate <- rest.sites.abate %>% rowwise() %>% 
  mutate(wet_emissions_CO2eMgYr = sum(c(CH4_salt_CO2eMgYr, N2O_salt_CO2eMgYr, CH4_supra_CO2eMgYr, N2O_supra_CO2eMgYr))) #rowSums(rest.sites.abate[,c(8:13)], na.rm=T)

# join baseline and wetland emissions and removals
rest.sites.abate.df<-full_join(rest.sites.base,rest.sites.abate,by="FID_site")

# 25 years
# create dataframe per year for biomass removals
rsites.biomass.25.df <- cbind(AGB_sup_growth_CO2eMgYr_25.df, 
                              AGB_salt_growth_CO2eMgYr_25.df,
                             BGB_sup_growth_CO2eMgYr_25.df)
dim(rsites.biomass.25.df)
## upper
rsites.biomass.25.df.upper <- cbind(AGB_sup_growth_CO2eMgYr_25.df.upper, 
                                    AGB_salt_growth_CO2eMgYr_25.df.upper,
                             BGB_sup_growth_CO2eMgYr_25.df.upper)
##lower
rsites.biomass.25.df.lower <- cbind(AGB_sup_growth_CO2eMgYr_25.df.lower, 
                                    AGB_salt_growth_CO2eMgYr_25.df.lower,
                                   BGB_sup_growth_CO2eMgYr_25.df.lower)

#calculate biomass removals each year over 25 years
rsites.biomass.25 <- rsites.biomass.25.df[1:25] + rsites.biomass.25.df[26:50] + rsites.biomass.25.df[51:75]
dim(rsites.biomass.25)
colnames(rsites.biomass.25) <- paste0("Biomass_yr",1:25)
##upper
rsites.biomass.25.upper <- rsites.biomass.25.df.upper[1:25] + rsites.biomass.25.df.upper[26:50] + rsites.biomass.25.df.upper[51:75]
colnames(rsites.biomass.25.upper) <- paste0("Biomass_yr",1:25)
##lower
rsites.biomass.25.lower <- rsites.biomass.25.df.lower[1:25] + rsites.biomass.25.df.lower[26:50] + rsites.biomass.25.df.lower[51:75]
colnames(rsites.biomass.25.lower) <- paste0("Biomass_yr",1:25)

#calculate abatement each year over 25 years
rsites.abate.25 <- rsites.biomass.25[1:25] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.25) <- paste0("abate_yr",1:25)
head(rsites.abate.25)
##upper
rsites.abate.25.upper <- rsites.biomass.25.upper[1:25] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.25.upper) <- paste0("abate_yr",1:25)
head(rsites.abate.25.upper)
##lower
rsites.abate.25.lower <- rsites.biomass.25.lower[1:25] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.25.lower) <- paste0("abate_yr",1:25)
head(rsites.abate.25.lower)

#total abatement over 25 years
rsites.abate.25$abate_CO2eMg_25yrs <- rowSums(rsites.abate.25[1:25], na.rm = T)
sum(rsites.abate.25$abate_CO2eMg_25yrs)
rsites.abate.25.upper$abate_CO2eMg_25yrs <- rowSums(rsites.abate.25.upper[1:25], na.rm = T)
sum(rsites.abate.25.upper$abate_CO2eMg_25yrs)
rsites.abate.25.lower$abate_CO2eMg_25yrs <- rowSums(rsites.abate.25.lower[1:25], na.rm = T)
sum(rsites.abate.25.lower$abate_CO2eMg_25yrs)

#save data required for NPV analysis - carbon abatement per year
rsites.abate.25.2 <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.25)
colnames(rsites.abate.25.2) #check
write.csv(rsites.abate.25.2, file = "rest.sites.abatement.pa.25years.csv")
##upper
rsites.abate.25.2.upper <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.25.upper)
colnames(rsites.abate.25.2.upper) #check
write.csv(rsites.abate.25.2.upper, file = "rest.sites.abatement.pa.25years.upper.csv")
##lower
rsites.abate.25.2.lower <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.25.lower)
colnames(rsites.abate.25.2.lower) #check
write.csv(rsites.abate.25.2.lower, file = "rest.sites.abatement.pa.25years.lower.csv")


# 100 years
#calculate abatement each year over 100 years
rsites.biomass.100.df <- cbind(AGB_sup_growth_CO2eMgYr_100.df, 
                               AGB_salt_growth_CO2eMgYr_100.df,
                               BGB_sup_growth_CO2eMgYr_100.df)
dim(rsites.biomass.100.df)
rsites.biomass.100 <- rsites.biomass.100.df[1:100] + rsites.biomass.100.df[101:200] + rsites.biomass.100.df[201:300]
dim(rsites.biomass.100)
colnames(rsites.biomass.100) <- paste0("Biomass_yr",1:100)

rsites.abate.100 <- rsites.biomass.100[1:100] + rest.sites.abate.df$wet_SOC_removals_CO2eMgYr -
  rest.sites.abate.df$wet_emissions_CO2eMgYr + rest.sites.abate.df$base_net_emissions_CO2eMgYr
colnames(rsites.abate.100) <- paste0("abate_yr",1:100)
head(rsites.abate.100)

#total abatement over 25 years and 100 years
rsites.abate.100$abate_CO2eMg_25yrs <- rowSums(rsites.abate.100[1:25], na.rm = T)
rsites.abate.100$abate_CO2eMg_100yrs <- rowSums(rsites.abate.100[26:100], na.rm = T)
sum(rsites.abate.100$abate_CO2eMg_25yrs)
sum(rsites.abate.100$abate_CO2eMg_100yrs)

#save data required for NPV analysis - carbon abatement per year
rsites.abate.100.2 <- cbind(FID_site=rest.sites.abate.df$FID_site, rsites.abate.100)
colnames(rsites.abate.100.2) #check
write.csv(rsites.abate.100.2, file = "rest.sites.abatement.pa.100yrs.csv")


#totals for each activity to include in manuscript
#but include total AGB and BGB for mangrove, saltmarsh and supratidal so can average
## calculate total abatement per year over 25 years for mang and salt

rest.sites.abate.df2 <- cbind(rest.sites.abate.df, rsites.biomass.25.df, abate_CO2eMg_25yrs=rsites.abate.25$abate_CO2eMg_25yrs, abate_CO2eMg_100yrs=rsites.abate.100$abate_CO2eMg_100yrs)
colnames(rest.sites.abate.df2)
#no mangrove for PHSW
rest.sites.abate.df2$AGB_sup_CO2eMg_25yr <- rest.sites.abate.df2 %>%
  dplyr::select(num_range("AGB_sup_yr", 1:25)) %>% 
  rowSums()
sum(rest.sites.abate.df2$AGB_sup_CO2eMg_25yr)
#67576
sum(rest.sites.abate.df2$AGB_salt_1yr_CO2eMgYr) #should be the same
#296
sum(rest.sites.abate.df2$AGB_salt_yr1)
#296

#no mangrove
rest.sites.abate.df2$BGB_sup_CO2eMg_25yr <- rest.sites.abate.df2 %>%
  dplyr::select(num_range("BGB_sup_yr", 1:25)) %>% 
  rowSums()
sum(rest.sites.abate.df2$BGB_sup_CO2eMg_25yr)
#18246

# itemise abatement from activities to include in manuscript
rest.sites.abate.sum <- rest.sites.abate.df2 %>% 
  dplyr::select(saltmarsh_area_ha.x, supratidal_area_ha.x, AGB_sup_CO2eMg_25yr, AGB_salt_1yr_CO2eMgYr, BGB_sup_CO2eMg_25yr,
        CH4_ponds_CO2eMgYr, N2O_forestland_Mel_CO2eMgYr, N2O_grazing_CO2eMgYr, SOC_loss_CO2eMgYr, SOC_deg_wet_CO2eMgYr,  
         base_net_emissions_CO2eMgYr,
         SOC_salt_CO2eMgYr, SOC_supra_CO2eMgYr, 
         wet_SOC_removals_CO2eMgYr,
         CH4_salt_CO2eMgYr, CH4_supra_CO2eMgYr, 
         N2O_salt_CO2eMgYr, N2O_supra_CO2eMgYr,
         wet_emissions_CO2eMgYr,
         abate_CO2eMg_25yrs, abate_CO2eMg_100yrs)
rest.sites.abate.sum <- apply(rest.sites.abate.sum,2, sum, na.rm=T) %>% as.matrix
write.csv(rest.sites.abate.sum, file = "rest.sites.abate.activites.totals.csv")






