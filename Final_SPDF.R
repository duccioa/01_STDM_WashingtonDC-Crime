library(sp)
library(rgdal)
library(plyr)
col_palette = c('orangered', 'plum1')


dc = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
names(dc@data)[3] = 'CENSUS_TRACT'
dc@data = dc@data[,-c(2,4,5,6,7,8, 9)]
load('./data/CrimeCensus_merged.RData')
census = read.csv('./data/ACS/census_fullset_for_ML.csv')
census = census[,-c(1:4)]
census = census[complete.cases(census),]
census$CENSUS_TRACT = as.factor(formatC(census$CENSUS_TRACT, width = 6, flag = 00))
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

dc@data = join(dc@data, aggr2spdf)
save(dc, file = './data/final_sp.RData')
