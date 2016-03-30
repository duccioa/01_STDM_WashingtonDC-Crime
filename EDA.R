library(ggplot2)
library(xts)
library(lubridate)
library(data.table)
require(GISTools)
require(spdep)
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
crime2013$CUM_WEEK = as.numeric(as.character(crime2013$WEEK)) + 53*2
crime2014$CUM_WEEK = as.numeric(as.character(crime2014$WEEK)) + 53*3
crime2011$CUM_YDAY = as.numeric(as.character(crime2011$YDAY))
crime2012$CUM_YDAY = as.numeric(as.character(crime2012$YDAY)) + 366
crime2013$CUM_YDAY = as.numeric(as.character(crime2013$YDAY)) + 366*2
crime2014$CUM_YDAY = as.numeric(as.character(crime2014$YDAY)) + 366*3
crime2011$CUM_MONTH = as.numeric(as.character(crime2011$MONTH))
crime2012$CUM_MONTH = as.numeric(as.character(crime2012$MONTH)) + 12
crime2013$CUM_MONTH = as.numeric(as.character(crime2013$MONTH)) + 12*2
crime2014$CUM_MONTH = as.numeric(as.character(crime2014$MONTH)) + 12*3

train = rbind(crime2011,crime2012, crime2013, crime2014)


# Clean the data 
table(train$YEAR) # there are some years that are not within the expected interval
train = train[train$YEAR == 2011 | 
                  train$YEAR == 2012 |
                  train$YEAR == 2013 | train$YEAR == 2014, ] # select 2012, 2013 and 2014 only
train = train[order(train$START_DATE),]
write.csv(train, './data/crime.csv')
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
g = g + xlab('Date') + ylab('Number of offenses') + ggtitle('Number of daily offenses, Washington DC 2011-2014')
g = g + scale_x_discrete(breaks = c(0, 30*6, 365, 365+(6*30),365*2,365*2+(6*30), 365*3, 365*3+(3*60), 365*4), 
                         labels = c('Jan-2011', 'June-2011','Jan-2012', 'June-2012','Jan-2013','June-2013','Jan-2014', 'June-2014', '2015'))
g + theme_bw()
ggsave('./Figures/NumOffenses_total.png')

# Plot offense daily rate
ggplot(summ_dt, aes(x = OFFENSE, y = DailyCount/days_in_month, 
                    group = OFFENSE, fill= OFFENSE)) + ggtitle('Average daily rate')+
    geom_boxplot() + ylab('Average daily count')
# Plot months' daily rate
off = unique(summ_dt$OFFENSE)
ggplot(summ_dt[OFFENSE == off[1]], aes(x = season, y = DailyCount/days_in_month, 
                    group = season, fill = season)) + ggtitle(off[1]) + 
    ylab('Daily crime rate') + xlab('') + theme(legend.position = 'none') +
    geom_boxplot()
ggsave('./Figures/season_dailyRate_TheftOther.png')
ggplot(summ_dt[OFFENSE == off[5]], aes(x = season, y = DailyCount/days_in_month, 
                                       group = season, fill = season)) + ggtitle(off[5]) + 
    ylab('Average daily crime rate') + xlab('') + theme(legend.position = 'none') +
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
train_d = table(train$CUM_YDAY[train$OFFENSE=='BURGLARY'])
png('./Figures/ACF_crime_day.png', width = 1200, height = 800)
par(mfrow=c(1,2))
acf(train_d, lag.max = 366*3, xlab = '', xaxt='n', main = '',
    sub = 'Lag max = 4 years',  cex.sub = 2, cex.axis = 1.6)
title(main = 'Crime daily count - ACF', cex.main = 2)
axis(side = 1, at = c(0,365/2, 365, (365/2)*3, 365*2,(365/2)*5, 365*3,(365/2)*7, 365*4), 
     labels = c(0, '', '1 yr', '', '2 yrs', '', '3 yrs', '', '4 yrs'), cex.axis = 1.6)
acf(train_d, lag.max = 90, main = '', 
    xlab = '', ylab = '', yaxt = 'n',
    sub = 'Lag max = 90 days', cex.axis = 1.6, cex.sub = 2)
dev.off()



png('./Figures/PACF_train_day.png')
pacf(train_d, lag.max = 365*3,  xlim= c(0,60),main = 'Crime daily count - PACF', xlab = 'Lag in days')
dev.off()


# Spatial autocorrelation
load('./data/final_sp.RData')
data = data.table(dc@data)
# Summer 2011
bur = data[OFFENSE == 'BURGLARY',.(YEAR, Count, CENSUS_TRACT, SUMMER, WINTER, AUTUMN, SPRING)]
season = bur[YEAR %in% c(2011, 2012, 2013) & SUMMER == 1, .(AVG_SEASONAL_COUNT = round(mean(Count))), by = CENSUS_TRACT]
bur$CENSUS_TRACT = as.character(bur$CENSUS_TRACT)
missing = "007301"
season = data.frame(season)
tmp = data.frame(CENSUS_TRACT = "007301", AVG_SEASONAL_COUNT=6)
season = rbind(season, tmp)
dc1 = dc
dc1@data = season
dc1@data$CENSUS_TRACT = as.character(dc1@data$CENSUS_TRACT)

tracts_crime = unique(bur$CENSUS_TRACT);tracts_crime = tracts_crime[order(tracts_crime)]
dctest = dc1[dc1$CENSUS_TRACT %in% tracts_crime,]
dc1_lw <- nb2listw(poly2nb(dctest), zero.policy=TRUE)
dc1_lI <- localmoran(dctest@data$AVG_SEASONAL_COUNT, dc1_lw, zero.policy = T)
dctest@data = cbind(dctest@data, dc1_lI)


png('./Figures/crimemap_LocalMoran.png', width = 800, height = 800)
plot_spdf_byClassInt(dctest, "Ii", col_pal = 'BrBG', 
                     legend_title = 'lI',
                     main_title = 'Local Moran of crime occurrence',
                     border_col = "black",
                     manual_int = F,
                     n_breaks = 8,
                     style_intervals = 'quantile',
                     subtitle = '', 
                     legend_size = 1.1)
plot(dc1, add = T, lwd = .4)
dev.off()

png("./Figures/crimemap_LocalMoranpvalues.png", width = 800, height = 800)
par(lwd = 0.0001)
pval_shade <- shading(c(0.01, 0.05, 01), cols = rev(brewer.pal(4, "PuRd")))
choropleth(dctest, dc1_lI[,5], shading = pval_shade, lwd = .00001, border = "white")
title("Local Moran of crime occurrence [Local p-values]", cex.main = 1)
choro.legend('bottomright', sh = pval_shade, cex = 1, title = "p-values")
plot(dc1, add = T, lwd = .4)
dev.off()
