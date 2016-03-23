library(lubridate)
library(data.table)
library(ggplot2)
source('STDM_functions.R')
####LOAD CENSUS
#df = read.csv('./data/FINAL_CrimeCensus_merged.csv')
#df = df[,-c(1, 11, 12, 13)]
#df$START_DATE = ymd_hms(df$START_DATE)
#df$YEAR = as.factor(df$YEAR)
#df$WEEK = as.factor(df$WEEK)
#df$MONTH = as.factor(df$MONTH)
#summary(df)
#save(df, file = './data/CrimeCensus_merged.RData')
load('./data/CrimeCensus_merged.RData')
census = read.csv('./data/ACS/census_fullset_for_ML.csv')
census = census[,-c(1:4)]
census$CENSUS_TRACT = as.factor(formatC(census$CENSUS_TRACT, width = 6, flag = 00))
df = data.table(df)
df$SEASON = df[,.(SEASON = ifelse(df$MONTH %in% c(9,10,11), 'AUTUMN', 
                         ifelse(df$MONTH %in% c(6,7,8), 'SUMMER', 
                                ifelse(df$MONTH %in% c(3,4,5), 'SPRING', 'WINTER'))))]
# Classification
aggr1 = df[, .(Count = .N), by = .(OFFENSE, CENSUS_TRACT, SEASON, YEAR)]
aggr1 = aggr1[OFFENSE == 'THEFT/OTHER']
hist(aggr1$Count, breaks = 176)
abline(v = mean(aggr1$Count), col = 'red')
abline(v = median(aggr1$Count), col = 'blue')
abline(v = quantile(aggr1$Count, c(.33, .66, 1)), col = 'green')
q = quantile(aggr1$Count, c(.33, .66, 1))
aggr1$LABEL = aggr1[,.(LABEL = ifelse(aggr1$Count %in% 0:q[1], 'LOW', 
                                       ifelse(aggr1$Count %in% (q[1]+1):q[2], 'AVERAGE', 'HIGH')))]
#SVM
aggr2 = aggr1[YEAR %in% c('2011', '2012', '2013')]
aggr3 = data.frame(aggr2)[,2:5]
Xtraining = join(aggr3, census, type = 'left')
Ytraining = aggr2$LABEL
test = aggr1[YEAR %in% '2014']
