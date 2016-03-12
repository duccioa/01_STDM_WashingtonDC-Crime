library(ggplot2)
library(xts)
library(lubridate)
source('STDM_functions.R')
source('FUN_acf_confInt.R')
# Plot variables
title_size = 3

crime2012 = read.csv('./data/Crime_Incidents__2012.csv', stringsAsFactors = FALSE)
crime2013 = read.csv('./data/Crime_Incidents__2013.csv', stringsAsFactors = FALSE)
crime2014 = read.csv('./data/Crime_Incidents__2014.csv', stringsAsFactors = FALSE)
crime2015 = read.csv('./data/Crime_Incidents__2015.csv', stringsAsFactors = FALSE)

crime2012 = prepare.dataset(crime2012)
crime2013 = prepare.dataset(crime2013)
crime2014 = prepare.dataset(crime2014)
crime2015 = prepare.dataset(crime2015)

crime2012$CUM_WEEK = crime2012$WEEK
crime2013$CUM_WEEK = crime2013$WEEK + 53
crime2014$CUM_WEEK = crime2014$WEEK + 106
crime2012$CUM_YDAY = crime2012$YDAY
crime2013$CUM_YDAY = crime2013$YDAY + 366
crime2014$CUM_YDAY = crime2014$YDAY + 366*2
crime2012$CUM_MONTH = crime2012$MONTH
crime2013$CUM_MONTH = crime2013$MONTH + 12
crime2014$CUM_MONTH = crime2014$MONTH + 12*2

train = rbind(crime2012, crime2013, crime2014)



names(crime2015)
head(crime2015)

### Plot type of offense frequency
# Training set
xt = table(train$OFFENSE)
xt = data.frame(xt)
xt = xt[order(-xt$Freq),]
names(xt) = c('Offense', 'Count')
g = plot.offenses(xt, '- Years 2012-2014')
save.2figures(g, 'Freq_offenses_trainset.png', leg = F)
# 2014
x14 = table(crime2014$OFFENSE)
x14 = data.frame(x14)
x14 = x14[order(-x14$Freq),]
names(x14) = c('Offense', 'Count')
g = plot.offenses(x14, 2014)
save.2figures(g, 'Freq_offenses2014.png', leg = F)
# 2015
x15 = table(crime2015$OFFENSE)
x15 = data.frame(x15)
x15 = x15[order(-x15$Freq),]
names(x15) = c('Offense', 'Count')
g = plot.offenses(x15, 2015)
save.2figures(g, 'Freq_offenses2015.png', leg = F)
# 


# Auto-correlation
train_w = table(train$CUM_WEEK)
w_acf = acf(train_w, lag.max = 53*3)
w_p = ggplot.acf(w_acf, main_title='ACF by week, training set')
save.2figures(w_p, 'ACF_train_week.png')

train_m = table(train$CUM_MONTH)
m_acf = acf(train_m, lag.max = 12*3)
m_p = ggplot.acf(m_acf, main_title='ACF by month, training set')
save.2figures(m_p, 'ACF_train_month.png')

train_d = table(train$CUM_YDAY)
d_acf = acf(train_d, lag.max = 366*3)
d_p = ggplot.acf(d_acf, main_title='ACF by day, training set')
save.2figures(d_p, 'ACF_train_day.png')



