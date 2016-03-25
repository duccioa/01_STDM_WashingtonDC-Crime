library(plyr)
library(ggplot2)
library(data.table)
load('./data/final_sp.RData')
load('./Functions/STDM_functions.R')
col_names = names(dc@data)




data = dc@data
sub2014 = data$YEAR == 2012 & data$WINTER == 1 & data$OFFENSE == 'BURGLARY'
dc1 = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
names(dc1@data)[3] = 'CENSUS_TRACT'
dc1@data = join(dc1@data, data)

plot_spdf_byClassInt(dc1, 'DP03_PCT_VC13', col_pal = 'Reds', 
                     legend_title = 'Unemployment rate',
                     main_title = 'Unemployment rate from ACS data',
                     subtitle = 'Washington DC, 2014')




data1 = data.table(data)
subOFF = data1[OFFENSE == 'BURGLARY']
g = ggplot(data1, aes(x = YEAR, y = Count, color = CENSUS_TRACT, group = CENSUS_TRACT)) + geom_line()
