library(sp)
library(rgdal)
library(plyr)
col_palette = c('orangered', 'plum1')


dc = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
# Winter 2011
y = 2011
ssn = 'WINTER'
crime_label = aggr1[YEAR == y & SEASON == ssn]
crime_label = data.frame(crime_label)
names(crime_label)[2] = 'TRACTCE'
merged = join(dc@data, crime_label)
dc_ssn = dc
dc_ssn@data = merged
off = as.character(dc_ssn@data$OFFENSE[1])
off = gsub('/', '-', off)
png(paste0('./Figures/','RateCrime_', off, '_',ssn, '_', y, '.png'), width = 1200, height = 1000)
plot(dc_ssn, col = col_palette[dc_ssn@data$LABEL],lwd = .2)
title(main = paste0('Rate of crime: ', off), sub = paste(tolower(ssn), ' ',y,', Washington DC', sep = ''), 
      cex.main = 2.5, cex.sub = 2)
legend('bottomright', legend = c('Above median', 'Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
dev.off()
# Summer 2011
y = 2011
ssn = 'SUMMER'
crime_label = aggr1[YEAR == y & SEASON == ssn]
crime_label = data.frame(crime_label)
names(crime_label)[2] = 'TRACTCE'
merged = join(dc@data, crime_label)
dc_ssn = dc
dc_ssn@data = merged
off = as.character(dc_ssn@data$OFFENSE[1])
off = gsub('/', '-', off)
png(paste0('./Figures/','RateCrime_', off, '_',ssn, '_', y, '.png'), width = 1200, height = 1000)
plot(dc_ssn, col = col_palette[dc_ssn@data$LABEL],lwd = .2)
title(main = paste0('Rate of crime: ', off), sub = paste(tolower(ssn), ' ',y,', Washington DC', sep = ''), 
      cex.main = 2.5, cex.sub = 2)
legend('bottomright', legend = c('Above median', 'Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
dev.off()

# Winter 2013
y = 2013
ssn = 'WINTER'
crime_label = aggr1[YEAR == y & SEASON == ssn]
crime_label = data.frame(crime_label)
names(crime_label)[2] = 'TRACTCE'
merged = join(dc@data, crime_label)
dc_ssn = dc
dc_ssn@data = merged
off = as.character(dc_ssn@data$OFFENSE[1])
off = gsub('/', '-', off)
png(paste0('./Figures/','RateCrime_', off, '_',ssn, '_', y, '.png'), width = 1200, height = 1000)
plot(dc_ssn, col = col_palette[dc_ssn@data$LABEL],lwd = .2)
title(main = paste0('Rate of crime: ', off), sub = paste(tolower(ssn), ' ',y,', Washington DC', sep = ''), 
      cex.main = 2.5, cex.sub = 2)
legend('bottomright', legend = c('Above median', 'Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
dev.off()
# Summer 2013
y = 2013
ssn = 'SUMMER'
crime_label = aggr1[YEAR == y & SEASON == ssn]
crime_label = data.frame(crime_label)
names(crime_label)[2] = 'TRACTCE'
merged = join(dc@data, crime_label)
dc_ssn = dc
dc_ssn@data = merged
off = as.character(dc_ssn@data$OFFENSE[1])
off = gsub('/', '-', off)
png(paste0('./Figures/','RateCrime_', off, '_',ssn, '_', y, '.png'), width = 1200, height = 1000)
plot(dc_ssn, col = col_palette[dc_ssn@data$LABEL],lwd = .2)
title(main = paste0('Rate of crime: ', off), sub = paste(tolower(ssn), ' ',y,', Washington DC', sep = ''), 
      cex.main = 2.5, cex.sub = 2)
legend('bottomright', legend = c('Above median', 'Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
dev.off()


# Prediction
y = 2013
ssn = 'SUMMER'
crime_label = aggr1[YEAR == y & SEASON == ssn]
crime_label = data.frame(crime_label)
names(crime_label)[2] = 'TRACTCE'
merged = join(dc@data, crime_label)
dc_ssn = dc
dc_ssn@data = merged
off = as.character(dc_ssn@data$OFFENSE[1])
off = gsub('/', '-', off)
png(paste0('./Figures/','RateCrime_', off, '_',ssn, '_', y, '.png'), width = 1200, height = 1000)
plot(dc_ssn, col = col_palette[dc_ssn@data$LABEL],lwd = .2)
title(main = paste0('Rate of crime: ', off), sub = paste(tolower(ssn), ' ',y,', Washington DC', sep = ''), 
      cex.main = 2.5, cex.sub = 2)
legend('bottomright', legend = c('Above median', 'Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
dev.off()