library(ggplot2)
library(xts)
library(lubridate)
library(data.table)
source('./Functions/STDM_functions.R')
source('./Functions/FUN_acf_confInt.R')
# Plot variables
title_size = 3



###################################################
crime2011 = read.csv('./data/DC_crime_2011.csv', stringsAsFactors = F)
crime2012 = read.csv('./data/Crime_Incidents__2012.csv', stringsAsFactors = FALSE)
crime2013 = read.csv('./data/Crime_Incidents__2013.csv', stringsAsFactors = FALSE)
crime2014 = read.csv('./data/Crime_Incidents__2014.csv', stringsAsFactors = FALSE)

crime2012 = prepare.dataset(crime2012, y = 2012)
crime2013 = prepare.dataset(crime2013, y = 2013)
crime2014 = prepare.dataset(crime2014, y = 2014)
crime2011 = prepare.dataset(crime2011, T, y = 2011)


crime2011$CUM_WEEK = as.numeric(as.character(crime2011$WEEK))
crime2012$CUM_WEEK = as.numeric(as.character(crime2012$WEEK)) + 53
crime2013$CUM_WEEK = as.numeric(as.character(crime2013$WEEK)) + 106
crime2011$CUM_YDAY = as.numeric(as.character(crime2011$YDAY))
crime2012$CUM_YDAY = as.numeric(as.character(crime2012$YDAY)) + 366
crime2013$CUM_YDAY = as.numeric(as.character(crime2013$YDAY)) + 366*2
crime2011$CUM_MONTH = as.numeric(as.character(crime2011$MONTH))
crime2012$CUM_MONTH = as.numeric(as.character(crime2012$MONTH)) + 12
crime2013$CUM_MONTH = as.numeric(as.character(crime2013$MONTH)) + 12*2

train = rbind(crime2011,crime2012, crime2013)


# Clean the data 
table(train$YEAR) # there are some years that are not within the expected interval
train = train[train$YEAR == 2011 | 
                  train$YEAR == 2012 |
                  train$YEAR == 2013, ] # select 2012, 2013 and 2014 only
train = train[order(train$START_DATE),]
write.csv(train, 'train.csv')
###################################################



dt = data.table(train)
summ_dt = dt[, .(DailyCount = .N), by = .(DAY, MONTH, CUM_MONTH, CUM_YDAY, OFFENSE)]

summ_dt[, days_in_month := ifelse(MONTH == 2 | 
                                 MONTH == 4 |
                                 MONTH == 6 | 
                                 MONTH == 9 |
                                 MONTH == 11, ifelse(MONTH == 2, 28, 30), 31)]
summ_dt[, season := 'WINTER'] 
summ_dt[MONTH == 3 | MONTH == 4 | MONTH == 5, season := 'SPRING']
summ_dt[MONTH == 6 | MONTH == 7 | MONTH == 8, season := 'SUMMER']
summ_dt[MONTH == 9 | MONTH == 10 | MONTH == 11, season := 'AUTUMN']

# Plot crime offenses by day
date_seq = seq.Date(from = as.Date('2011-01-01'), to = as.Date('2013-12-31'), 'days')
off_day = summ_dt[OFFENSE %in% c('BURGLARY', 'ROBBERY', 'THEFT F/AUTO',
                                 'THEFT/OTHER', 'MOTOR VEHICLE THEFT')]
g = ggplot(off_day, aes(x = CUM_YDAY,y = DailyCount,group = OFFENSE, colour = OFFENSE)) + geom_smooth()
g = g + xlab('Date') + ylab('Number of offenses') + ggtitle('Number of daily offenses, Washington DC 2012-2014')
g = g + scale_x_discrete(breaks = c(0, 30*6, 365, 365+(6*30),365*2,365*2+(6*30), 365*3), 
                         labels = c('Jan-2012', 'June-2012','Jan-2013', 'June-2013','Jan-2014','June-2014','2015'))
g
ggsave('./Figures/NumOffenses_total.png')

# Plot offense daily rate
ggplot(summ_dt, aes(x = OFFENSE, y = DailyCount/days_in_month, 
                    group = OFFENSE, fill= OFFENSE)) +
    geom_boxplot()
# Plot months' daily rate
off = unique(summ_dt$OFFENSE)
ggplot(summ_dt[OFFENSE == off[1]], aes(x = season, y = DailyCount/days_in_month, 
                    group = season, fill = season)) + ggtitle(off[1]) + 
    ylab('Daily crime rate') + xlab('') + theme(legend.position = 'none') +
    geom_boxplot()
ggsave('./Figures/season_dailyRate_TheftOther.png')
ggplot(summ_dt[OFFENSE == off[5]], aes(x = season, y = DailyCount/days_in_month, 
                                       group = season, fill = season)) + ggtitle(off[5]) + 
    ylab('Daily crime rate') + xlab('') + theme(legend.position = 'none') +
    geom_boxplot()
ggsave('./Figures/season_dailyRate_Burglary.png')
ggplot(summ_dt[OFFENSE == off[6]], aes(x = season, y = DailyCount/days_in_month, 
                                       group = season, fill = season)) + ggtitle(off[6]) + 
    ylab('Daily crime rate') + xlab('') + theme(legend.position = 'none') +
    geom_boxplot()
ggsave('./Figures/season_dailyRate_Robbery.png')




### Plot type of offense frequency
# Training set
xt = table(train$OFFENSE)
xt = data.frame(xt)
xt = xt[order(-xt$Freq),]
names(xt) = c('Offense', 'Count')
g = plot.offenses(xt, '- Years 2011-2013')
save.2figures(g, 'Freq_offenses_trainset.png', leg = F)
# 2014
x14 = table(crime2014$OFFENSE)
x14 = data.frame(x14)
x14 = x14[order(-x14$Freq),]
names(x14) = c('Offense', 'Count')
g = plot.offenses(x14, 2014)
save.2figures(g, 'Freq_offenses2014.png', leg = F)


# Auto-correlation
train_d = table(train$CUM_YDAY)
png('./Figures/ACF_train_day.png')
acf(train_d, lag.max = 366*3)
dev.off()
png('./Figures/ACF_train_day_lagMonth.png')
acf(train_d, lag.max = 30)
dev.off()
png('./Figures/PACF_train_day.png')
pacf(train_d, lag.max = 365*3)
dev.off()
png('./Figures/PACF_train_day_lagMonth.png')
pacf(train_d, lag.max = 30)
dev.off()


