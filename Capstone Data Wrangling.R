## Data Wrangling 

#load libraries

library(dplyr)
library(tidyr)
library(ggplot2)

#Medicare Charge Data from 2014
medicare.orig <- read.csv('Data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv')
medicare.orig$Provider.Id <- factor(medicare.orig$Provider.Id)
medicare.orig$Provider.Zip.Code <- factor(medicare.orig$Provider.Zip.Code)

#Include Census Regions in Medicare Charge Data
regions <- read.csv('Data/censusregions.csv')

medicare.large <- merge(medicare.orig,regions,by.x="Provider.State", by.y = "State.Code")

#Add additional data about hospitals
hospitals <- read.csv("Data/Hospital_General_Information.csv")
hospitals2 <- hospitals %>% select(1,2,7,10,13,15,17, 19, 21, 23, 25)
names(hospitals2)[names(hospitals2) == 'Provider.ID'] <- 'Provider.Id'

medicare.large <- left_join(medicare.large, hospitals2)


# Add fips county code, latitide, and longitude for each zip code
library(noncensus)
data(zip_codes)
zip_codes$zip <- as.factor(zip_codes$zip)
zip_codes$fips <- as.factor(zip_codes$fips)
zip_codes2 <- select(zip_codes, 1, 4:6)

names(zip_codes2)[names(zip_codes2) == 'fips'] <- 'County.ID'
names(zip_codes2)[names(zip_codes2) == 'zip'] <- 'Provider.Zip.Code'

medicare.large <- left_join(medicare.large, zip_codes2)

#Add percent in poverty and median income by county
poverty <- read.csv('Data/Poverty by County.csv')
poverty2 <- select(poverty, 3, 10, 41)
poverty2$County.ID <- as.factor(poverty2$County.ID)
names(poverty2)[names(poverty2) == 'All.Ages.in.Poverty.Percent'] <- 'Poverty.Percent.County'
names(poverty2)[names(poverty2) == 'Median.Household.Income.in.Dollars'] <- 'Median.Income.County'


medicare.large <- left_join(medicare.large, poverty2)

#Add county unemployment rate
unemployment <- read.csv("Data/UnempRate.csv")
names(unemployment)[names(unemployment) == 'Unemployment.Rate'] <- 'Unemployment.County'

FIPS <- read.csv("Data/fips_codes_website.csv")
FIPS2 <- select(FIPS, 2:4)
names(FIPS2)[names(FIPS2) == 'FIPS.Entity.Code'] <- 'County.ID'
FIPS2$County.ID <- as.factor(FIPS2$County.ID)

unemployment <- left_join(unemployment,FIPS2)

medicare.large <- left_join(medicare.large, unemployment)



#Add county density
countydensity <- read.csv("Data/CountyDensity.csv")
countydensity2 <- select(countydensity, 5, 13)
names(countydensity2)[names(countydensity2) == 'Target.Geo.Id2'] <- 'County.ID'
names(countydensity2)[names(countydensity2) == 'Density.per.square.mile.of.land.area...Population'] <- 'Density.County'
countydensity2$County.ID <- as.factor(countydensity2$County.ID)

medicare.large <- left_join(medicare.large, countydensity2)


#Separate DRG Code and Defintion

medicare.large <- separate(medicare.large, col = DRG.Definition, into = c("DRG.Code", "DRG.Name"), sep = " - ")


#Create new table with only variables useful for analysis. Get rid of redundant variables.
medicare.full <- select(medicare.large, 2:5, 7:8, 10:31, 41:42)


#Make sure everything is the right data type
medicare.full$DRG.Code <- as.factor(medicare.full$DRG.Code)
medicare.full$DRG.Name <- as.factor(medicare.full$DRG.Name)
medicare.full$Provider.Id <- as.factor(medicare.full$Provider.Id)
medicare.full$Provider.Zip.Code <- as.factor(medicare.full$Provider.Zip.Code)
medicare.full$County.ID <- as.factor(medicare.full$County.ID)

#Create new column - Proportion of Average.Total.Payments payed by medicare

medicare.full$Prop.Covered.by.Medicare <- (medicare.full$Average.Medicare.Payments / medicare.full$Average.Total.Payments)


#Create mini tables for most common DRG defintions: 
# 1.)     194 - SIMPLE PNEUMONIA & PLEURISY W CC
# 2.)     871 - SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC 
# 3.)     292 - HEART FAILURE & SHOCK W CC
# 4.)     392 - ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC 
# 5.)     690 - KIDNEY & URINARY TRACT INFECTIONS W/O MCC 

medicare.pneumonia <- subset(medicare.full, DRG.Code == 194)
medicare.septicemia <- subset(medicare.full, DRG.Code == 871)
medicare.heart <- subset(medicare.full, DRG.Code == 292)
medicare.esophagitis <- subset(medicare.full, DRG.Code == 392)
medicare.kidney <- subset(medicare.full, DRG.Code == 690)

