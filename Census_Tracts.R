library(sp)
library(rgdal)
cen_tr = readOGR('./shp/cb_2014_11_tract_500k', 'cb_2014_11_tract_500k')
cen_tr@data$GEOID = as.character(cen_tr@data$GEOID)
names(cen_tr@data)[5] = 'GEO.id2'
