library(lubridate)
library(plyr)
library(sp)
library(rgdal)
source('./Functions/STDM_functions.R')
#### LOAD CENSUS ####
census = read.csv('./data/ACS/census_fullset_for_ML.csv', stringsAsFactors = F)
census = census[,-1]
census$CENSUS_TRACT = as.factor(formatC(census$CENSUS_TRACT, width = 6, flag = 0))
summary(census)
#### LOAD CRIME ####
crime2011 = read.csv('./data/DC_crime_2011.csv', stringsAsFactors = FALSE)
crime2012 = read.csv('./data/Crime_Incidents__2012.csv', stringsAsFactors = FALSE)
crime2013 = read.csv('./data/Crime_Incidents__2013.csv', stringsAsFactors = FALSE)
crime2014 = read.csv('./data/Crime_Incidents__2014.csv', stringsAsFactors = FALSE)

crime2011a = prepare.dataset(crime2011, T, 2011)
crime2012a = prepare.dataset(crime2012, F, 2012)
crime2013a = prepare.dataset(crime2013, F, 2013)
crime2014a = prepare.dataset(crime2014, F, 2014)

df_crime = rbind(crime2011a, crime2012a, crime2013a, crime2014a)
df_crime = df_crime[complete.cases(df_crime),]
df_crime$CENSUS_TRACT = formatC(df_crime$CENSUS_TRACT, width = 6, flag=0)

write.csv(df_crime, file = './data/FINAL_crime_full.csv')

rm(crime2014a, crime2013a, crime2012a, crime2011a)
rm(crime2014, crime2013, crime2012, crime2011)
#### JOIN ####
df = join(df_crime, census)
write.csv(df, './data/FINAL_CrimeCensus_merged.csv')
save(df, file = './data/CrimeCensus_merged.RData')
rm(df_crime)
#### Create SPDF ####
dc = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
names(dc@data)[3] = 'CENSUS_TRACT'
dc@data = dc@data[,-c(2,4,5,6,7,8, 9)]
load('./data/CrimeCensus_merged.RData')
census = read.csv('./data/ACS/census_fullset_for_ML.csv')
census = census[,-c(1:4)]
census$CENSUS_TRACT = formatC(census$CENSUS_TRACT, width = 6, flag = 00)
df = data.table(df)
df$SEASON = df[,.(SEASON = ifelse(df$MONTH %in% c(9,10,11), 'AUTUMN', ifelse(df$MONTH %in% c(6,7,8), 'SUMMER', ifelse(df$MONTH %in% c(3,4,5), 'SPRING', 'WINTER'))))]
df = data.table(data.frame(df)[,-c(2,4,6,7,8,9)])

aggr1 = df[, .(Count = .N), by = .(OFFENSE, CENSUS_TRACT, SEASON, YEAR)]
aggr1$WINTER = aggr1[,.(WINTER = ifelse(aggr1$SEASON == 'WINTER', 1, 0))]
aggr1$SPRING = aggr1[,.(SPRING = ifelse(aggr1$SEASON == 'SPRING', 1, 0))]
aggr1$SUMMER = aggr1[,.(SUMMER = ifelse(aggr1$SEASON == 'SUMMER', 1, 0))]
aggr1$AUTUMN = aggr1[,.(AUTUMN = ifelse(aggr1$SEASON == 'AUTUMN', 1, 0))]
aggr1 = join(data.frame(aggr1), census)## Data frame
aggr2spdf = aggr1[,-3]
write.csv(aggr2spdf, './data/aggr1.csv')
dc@data = join(dc@data, aggr2spdf)
save(dc, file = './data/final_sp.RData')
writeOGR(dc, dsn = './shp', layer = 'dc_merged', driver = 'ESRI Shapefile')
