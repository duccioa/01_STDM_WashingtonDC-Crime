library(plyr)
library(ggplot2)
library(data.table)
library(reshape2)
source('./data/final_sp.RData')
source('./Functions/STDM_functions.R')


load('./data/EDA.Rdata')
census  = census[complete.cases(census),]
census = data.table(census)

# unemployment
png('./Figures/EDA_unempl.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
unempl = census[,.(UNEMPL = mean(DP03_PCT_VC13)), by = CENSUS_TRACT]
max_unempl = as.character(unempl[max(UNEMPL), CENSUS_TRACT])
min_unempl = as.character(unempl[min(UNEMPL), CENSUS_TRACT])
series = census[CENSUS_TRACT == max_unempl, .(MAX = DP03_PCT_VC13, YEAR = as.character(YEAR))]
series = cbind(series, census[CENSUS_TRACT == min_unempl, .(MIN = DP03_PCT_VC13)])
plot(series$YEAR, series$MAX, type = 'l', col = 'red', ylim = c(0,18),
     main='Unemployment level per census tract', xlab = 'Year', ylab = '% of unemployed',
     sub = 'Washington, DC', xaxt = 'n', lwd = 3)
axis(side = 1, at = c(2011, 2012, 2013, 2014),labels = c(2011, 2012, 2013, 2014))
lines(series$YEAR, series$MIN, col = 'blue', lwd = 3)
legend('topright', c('Maximum values', 'Minimum values'), 
       lty = c(1,1),col = c('red', 'blue'), bty = 'n', y.intersp = 2,
       text.width = 2, lwd = 3)
dev.off()


# Income
png('./Figures/EDA_income.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
sub = census[,.(VALUE = mean(DP03_VALUE_VC85)), by = CENSUS_TRACT]
sub = sub[complete.cases(sub),]
sub = sub[order(VALUE),]
max_sub = as.character(sub[VALUE == max(sub$VALUE), CENSUS_TRACT])
min_sub = as.character(sub[5,CENSUS_TRACT])
series = census[CENSUS_TRACT == max_sub, .(MAX = DP03_VALUE_VC85, YEAR = as.character(YEAR))]
series = cbind(series, census[CENSUS_TRACT == min_sub, .(MIN = DP03_VALUE_VC85)])

plot(series$YEAR, series$MAX, type = 'l', col = 'green', 
     ylim = c(min(sub$VALUE)*1.05,max(sub$VALUE)*1.05),
     main='Median household income', xlab = 'Year', ylab = '$',
     sub = 'Washington, DC', xaxt = 'n', lwd = 3)
axis(side = 1, at = c(2011, 2012, 2013, 2014),labels = c(2011, 2012, 2013, 2014))
lines(series$YEAR, series$MIN, col = 'deeppink4', lwd = 3)
legend('right', c('Maximum values', 'Minimum values'), 
       lty = c(1,1),col = c('green', 'deeppink4'), bty = 'n', y.intersp = 2,
       text.width = 1, lwd = 3)
dev.off()

# Poverty level
png('./Figures/EDA_povLev.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
sub = census[,.(VALUE = mean(DP03_PCT_VC166)), by = CENSUS_TRACT]
sub = sub[complete.cases(sub),]
sub = sub[order(VALUE),]
max_sub = as.character(sub[nrow(sub),CENSUS_TRACT])
min_sub = as.character(sub[1,CENSUS_TRACT])
series = census[CENSUS_TRACT == max_sub, .(MAX = DP03_PCT_VC166, YEAR = as.character(YEAR))]
series = cbind(series, census[CENSUS_TRACT == min_sub, .(MIN = DP03_PCT_VC166)])

plot(series$YEAR, series$MAX, type = 'l', col = 'darkorange4', 
     ylim = c(min(sub$VALUE)*1.05,max(sub$VALUE)*1.2),
     main='Pct of families with income below poverty level', xlab = 'Year', ylab = '%',
     sub = 'Washington, DC', xaxt = 'n', lwd = 3)
axis(side = 1, at = c(2011, 2012, 2013, 2014),labels = c(2011, 2012, 2013, 2014))
lines(series$YEAR, series$MIN, col = 'darkorchid2', lwd = 3)
legend('right', c('Maximum values', 'Minimum values'), 
       lty = c(1,1),col = c('darkorange4', 'darkorchid2'), bty = 'n', y.intersp = 2,
       text.width = 1, lwd = 3)
dev.off()

# Age
png('./Figures/EDA_age.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
sub = census[,.(VALUE = mean(DP05_VALUE_VC21)), by = CENSUS_TRACT]
sub = sub[complete.cases(sub),]
sub = sub[order(VALUE),]
max_sub = as.character(sub[nrow(sub),CENSUS_TRACT])
min_sub = as.character(sub[5,CENSUS_TRACT])
series = census[CENSUS_TRACT == max_sub, .(MAX = DP05_VALUE_VC21, YEAR = as.character(YEAR))]
series = cbind(series, census[CENSUS_TRACT == min_sub, .(MIN = DP05_VALUE_VC21)])

plot(series$YEAR, series$MAX, type = 'l', col = 'firebrick3', 
     ylim = c(min(sub$VALUE)*1.05,max(sub$VALUE)*1.05),
     main='Median Age', xlab = 'Year', ylab = '%',
     sub = 'Washington, DC', xaxt = 'n', lwd = 3)
axis(side = 1, at = c(2011, 2012, 2013, 2014),labels = c(2011, 2012, 2013, 2014))
lines(series$YEAR, series$MIN, col = 'cyan', lwd = 3)
legend('right', c('Maximum values', 'Minimum values'), 
       lty = c(1,1),col = c('firebrick3', 'cyan'), bty = 'n', y.intersp = 2,
       text.width = 1, lwd = 3)
dev.off()


##### Maps
# Unemployment
png('./Figures/EDA_unempl_m.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
mdata = census[,.(UNEMPL = mean(DP03_PCT_VC13)), by = CENSUS_TRACT]
dc = dc1
dc1@data = join(dc1@data, mdata)
plot_spdf_byClassInt(dc1, 'UNEMPL', col_pal = 'Reds', 
                     legend_title = 'Unemployment rate',
                     main_title = 'Unemployment rate from ACS data',
                     border_col = "black",
                     subtitle = 'Washington DC, 2014')
dev.off()
# Median income
png('./Figures/EDA_income_m.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
sub = census[,.(VALUE = mean(DP03_VALUE_VC85)), by = CENSUS_TRACT]
dc = dc1
dc@data = join(dc@data, sub)
plot_spdf_byClassInt(dc, 'VALUE', col_pal = 'Blues', 
                     legend_title = 'Median Income',
                     main_title = 'Median household income from ACS data',
                     border_col = "black",
                     style_intervals = 'quantile',
                     n_breaks = 8,
                     subtitle = 'Washington DC, 2014')
dev.off()
# Povery level
png('./Figures/EDA_povLev_m.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
sub = census[,.(VALUE = mean(DP03_PCT_VC166)), by = CENSUS_TRACT]
sub = sub[complete.cases(sub),]
dc = dc1
dc@data = join(dc@data, sub)
plot_spdf_byClassInt(dc, 'VALUE', col_pal = 'PuBuGn', 
                     legend_title = 'Poverty level',
                     main_title = 'Percentage of households below poverty',
                     border_col = "black",
                     n_breaks = 5,
                     subtitle = 'Washington DC, 2014')
dev.off()
# Age
png('./Figures/EDA_age_m.png', width = 660, height = 620)
par(cex = 1.4, cex.main = 1.8)
sub = census[,.(VALUE = mean(DP05_VALUE_VC21)), by = CENSUS_TRACT]
sub = sub[complete.cases(sub),]
dc = dc1
dc@data = join(dc@data, sub)
plot_spdf_byClassInt(dc, 'VALUE', col_pal = 'YlOrBr', 
                     legend_title = 'Age',
                     main_title = 'Median age',
                     border_col = "black",
                     n_breaks = 6,
                     subtitle = 'Washington DC, 2014')
dev.off()

#Heat map
crime = read.csv('./data/crime.csv')
crime$CENSUS_TRACT = formatC(crime$CENSUS_TRACT, width = 6, flag = 0)
crime$CENSUS_TRACT = as.factor(crime$CENSUS_TRACT)
crime$YEAR = as.factor(crime$YEAR)
crime$CUM_MONTH = as.factor(crime$CUM_MONTH)
crime1 = data.frame(OFFENSE = as.factor(as.character(crime$OFFENSE)), 
                    CENSUS_TRACT = as.factor(as.character(crime$CENSUS_TRACT)), 
                    CUM_MONTH = as.factor(crime$CUM_MONTH))
crime1 = data.table(crime1)
bur = crime1[, .(COUNT = .N), by = .(CUM_MONTH,CENSUS_TRACT)]
lev = levels(bur$CENSUS_TRACT)
bur = data.frame(bur)
C = acast(bur, CENSUS_TRACT ~ CUM_MONTH)
C[is.na(C)] = 0
xlabs = c(paste(c('Jan', 'Feb', 'March', 'April', 'May', 'June',
                            'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'), 11),
              paste(c('Jan', 'Feb', 'March', 'April', 'May', 'June',
                            'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'), 12),
              paste(c('Jan', 'Feb', 'March', 'April', 'May', 'June',
                            'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'), 13),
              paste(c('Jan', 'Feb', 'March', 'April', 'May', 'June',
                            'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'), 14))
colnames(C) = xlabs
png('./Figures/crime_heatmap.png', width = 800, height = 800)
heatmap(C, Rowv=NA,Colv=NA, col=heat.colors(256, .9),scale="column",
        margins=c(4,4),xlab="",ylab="Census Tract", 
        cexCol=1.1,y.scale.components.subticks(n=20))
dev.off()

# Crime map

load('./data/final_sp.RData')
data = data.table(dc@data)
# Summer 2011
bur = data[OFFENSE == 'BURGLARY',.(YEAR, Count, CENSUS_TRACT, SUMMER, WINTER, AUTUMN, SPRING)]
season = bur[YEAR %in% c(2011, 2012, 2013) & SUMMER == 1, .(AVG_SEASONAL_COUNT = round(mean(Count))), by = CENSUS_TRACT]
season = data.frame(season)
dc1 = dc
dc1@data = season

png('./Figures/crimemap_AVG_summer.png', width = 800, height = 800)
plot_spdf_byClassInt(dc1, "AVG_SEASONAL_COUNT", col_pal = 'YlOrBr', 
                     legend_title = '',
                     main_title = 'Bulgrary, Average Count',
                     border_col = "black",
                     manual_int = F,
                     n_breaks = 4,
                     style_intervals = 'quantile',
                     subtitle = 'Average summer 2011-2012-2013', 
                     legend_size = 1.1)
plot(dc1, add = T, lwd = .4)
dev.off()

# Summer 2014
bur = data[OFFENSE == 'BURGLARY',.(YEAR, Count, CENSUS_TRACT, SUMMER, WINTER, AUTUMN, SPRING)]
season = bur[SUMMER == 1&YEAR == 2014, .(AVG_SEASONAL_COUNT = Count), by = CENSUS_TRACT]
season = data.frame(season)
dc1 = dc
dc1@data = season

png('./Figures/crimemap_summ2014.png', width = 800, height = 800)
plot_spdf_byClassInt(dc1, "AVG_SEASONAL_COUNT", col_pal = 'YlOrBr', 
                     legend_title = '',
                     main_title = 'Summer 2014',
                     border_col = "black",
                     subtitle = '',
                     n_breaks = 4,
                     manual_int = F,
                     style_intervals = 'quantile',
                     legend_size = 1.1)
plot(dc1, add = T, lwd = .4)
dev.off()