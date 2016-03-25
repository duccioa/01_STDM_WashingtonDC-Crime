library(lubridate)
library(plyr)
library(sp)
library(rgdal)
source('./Functions/STDM_functions.R')

census = read.csv('./data/ACS/census_fullset_for_ML.csv', stringsAsFactors = F)
census = census[,-1]
census$CENSUS_TRACT = formatC(census$CENSUS_TRACT, width = 6, flag = 00)
crime = read.csv('./data/FINAL_crime_full.csv')
dc = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
names(dc@data)[3] = 'CENSUS_TRACT'
dc@data = dc@data[,-c(2,4,5,6,7,8, 9)]


# Check NAs
census_na = census$CENSUS_TRACT[is.na(census$DP03_PCT_VC13)| is.na(census$DP03_PCT_VC166)]
dc_na = dc[dc@data$CENSUS_TRACT == '006202',] 
dc1 = dc
dc1@data = join(dc1@data, census[census$YEAR == '2014',])
plot_spdf_byClassInt(dc1, 'DP03_PCT_VC13')
census$DP03_PCT_VC13[is.na(census$DP03_PCT_VC13)] = 7.1 # Replace NAs with average of the neighbours
plot_spdf_byClassInt(dc1, 'DP03_PCT_VC166')
census$DP03_PCT_VC166[is.na(census$DP03_PCT_VC166)] = 10.9
write.csv(census, './data/ACS/census_fullset_for_ML.csv')


