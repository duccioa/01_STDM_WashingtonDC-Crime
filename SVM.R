library(kernlab)
library(plyr)
library(sp)
library(rgdal)
source('./Functions/STDM_functions.R')
census = read.csv('./data/ACS/census_fullset_for_ML.csv', stringsAsFactors = F)
census = census[,-1]
census$CENSUS_TRACT = formatC(census$CENSUS_TRACT, width = 6, flag = 00)
census$YEAR = as.character(census$YEAR)
svm_data = read.csv('./data/aggr1.csv')
dc = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
names(dc@data)[3] = 'CENSUS_TRACT'
dc@data = dc@data[,-c(2,4,5,6,7,8, 9)]




# SVM
yrs_training = c(2011)
yr_test = 2014
season_test = 2 ; season = 'SUMMER'# 1 for WINTER, 2 for SPRING, 3 for SUMMER, 4 for AUTUMN
off = 'BURGLARY'
n = 1:nrow(svm_data)
trainInd = n[svm_data$OFFENSE %in% off & svm_data$YEAR %in% yrs_training]
yTestInd = n[svm_data$OFFENSE %in% off & svm_data$YEAR %in% yr_test & svm_data[,season] == 1]
m = median(svm_data$Count[trainInd])
svm_data$LABEL = ifelse(svm_data$Count <= m, -1, 1)
X = as.matrix(svm_data[,-c(1,2,3,4,5, ncol(svm_data))])
y = as.matrix(svm_data$LABEL)
XTrain = X[trainInd,]
XTest = X[yTestInd,]
yTrain = y[trainInd]
yTest = y[yTestInd]


# SVM test 1
svm_type = "C-svc"
c_value = 10
ker = 'rbfdot' # polydot rbfdot
kp = 'automatic'
cross_val = 4
lsModel <- ksvm(x=XTrain, y=yTrain,type=svm_type, kernel = ker, kpar = kp, C = c_value, cross = cross_val)
lsPred <- predict(lsModel, XTest)
predErr <- length(which((lsPred-yTest)>0))/length(lsPred)

pltClass <- lsModel@fitted
pltClass[lsModel@SVindex] <- 2
brks <- c(-1,1,2)
lbls <- findInterval(pltClass, brks)
col_palette <- c("orangered", "grey", "plum1")
png(paste0('./Figures/SVM_', gsub('/', '-', off), '_', season, yr_test, '_C', as.character(c_value), '_', ker, '_', kp, '.png'),
           width = 1200, height = 1000)
plot(dc, col=col_palette[lbls], lwd = .5)
title(main = paste0('SVM Classification results: ', off), sub = paste0('Washington DC, ', season, ' ', yr_test), 
      cex.main = 2, cex.sub = 1.5)
legend('bottomright', legend = c('Above median', 'Support Vectors','Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
text(x = -76.90461, y = 38.985, labels = paste('C = ', c_value), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.97, labels = paste('Kernel = ', ker), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.955, labels = paste('Kpar = ', kp), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.93, labels = paste('Error = ', round(predErr, 3)), cex = 1.5, pos = 4)
dev.off()

