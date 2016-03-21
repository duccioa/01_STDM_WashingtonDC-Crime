library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrgram)
census = read.csv('./data/ACS/census_fullset_for_ML.csv', stringsAsFactors = FALSE)
summary(census)
train2 = read.csv('train.csv')
weather = read.csv('./Data/DC_weatherAVGs.csv')
head(weather)
weather = weather[,c(2,3,4,5,10,11,12,13,14,15,16,17,22,26,27)]
weather$Date = ymd(weather$Date)
train2$Date = ymd(paste(year(train2$START_DATE),month(train2$START_DATE), 
                        day(train2$START_DATE), sep='-'))
weather = data.table(weather)
train2 = data.table(train2)
train2 = train2[OFFENSE %in% c('BURGLARY', 'ROBBERY', 'THEFT F/AUTO',
                                'THEFT/OTHER', 'MOTOR VEHICLE THEFT')]

corr_aggr = train2[, .(DailyCount = .N), by = .(Date, OFFENSE, WDAY)] # aggregation for correlation plot
aggr_merged = inner_join(corr_aggr, weather)

g = ggplot(aggr_merged, aes(x = Date, y = DailyCount, group = OFFENSE, colour = OFFENSE)) + geom_smooth()
g = g + geom_line(aes(y = ZSCORE.Avg.Temp), colour = 'red')

weekday_burg = aggr_merged[OFFENSE %in% 'BURGLARY' & WDAY %in% 'WD']
pairs(~ DailyCount + Act.High + Act.Low + Act.Avg + ZSCORE.Avg.Temp + Precip.Amt + Snow.Amt, data = weekday_burg)
corrgram(weekday_burg, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="BURGLARY on weekdays")
we_burg = aggr_merged[OFFENSE %in% 'BURGLARY' & WDAY %in% 'WE']
corrgram(we_burg, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="BURGLARY on weekends")
fit = lm(DailyCount ~ ZSCORE.Avg.Temp, we_burg)




rob = aggr_merged[OFFENSE %in% 'ROBBERY' & WDAY %in% 'WD']
pairs(~ DailyCount + Act.High + Act.Low + Act.Avg + ZSCORE.Avg.Temp, data = rob)
corrgram(rob, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="ROBBERY on weekdays")
