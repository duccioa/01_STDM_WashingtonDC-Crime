library(lubridate)
library(plyr)
source('STDM_functions.R')
#LOAD CENSUS
census = read.csv('./data/ACS/census_fullset_for_ML.csv', stringsAsFactors = F)
census = census[,-1]
census$CENSUS_TRACT = as.factor(formatC(census$CENSUS_TRACT, width = 6, flag = 0))
summary(census)
#LOAD CRIME
crime2011 = read.csv('./data/DC_crime_2011.csv', stringsAsFactors = FALSE)
crime2012 = read.csv('./data/Crime_Incidents__2012.csv', stringsAsFactors = FALSE)
crime2013 = read.csv('./data/Crime_Incidents__2013.csv', stringsAsFactors = FALSE)
crime2014 = read.csv('./data/Crime_Incidents__2014.csv', stringsAsFactors = FALSE)

crime2011a = prepare.dataset(crime2011, T, 2011)
crime2012a = prepare.dataset(crime2012, F, 2012)
crime2013a = prepare.dataset(crime2013, F, 2013)
crime2014a = prepare.dataset(crime2014, F, 2014)

df_crime = rbind(crime2011a, crime2012a, crime2013a, crime2014a)
write.csv(df, './data/FINAL_crime_full.csv')
df_crime$CENSUS_TRACT = as.factor(formatC(df_crime$CENSUS_TRACT, width = 6, flag=0))
rm(crime2014a, crime2013a, crime2012a, crime2011a)
#JOIN
df = join(df_crime, census)
write.csv(df, './data/FINAL_CrimeCensus_merged.csv')



