setwd("/Users/sarahhank/Dropbox/CASA/STDM/Coursework/DATA")
library(tree)
library(rgdal)
source('./STDM_functions.R')
census = read.csv('./census_fullset_for_ML.csv', stringsAsFactors = F)
census = census[,-1]
census$CENSUS_TRACT = formatC(census$CENSUS_TRACT, width = 6, flag = 00)
census$YEAR = as.character(census$YEAR)
rf_data = read.csv('./aggr1.csv')
dc = readOGR('../SHP/', 'cb_2014_11_tract_500k')
names(dc@data)[3] = 'CENSUS_TRACT'
dc@data = dc@data[,-c(2,4,5,6,7,8, 9)]

#### Prepare for RF ####
yrs_training = c(2011,2012,2013)
yr_test = 2014
season_test = 3 ; season = 'SUMMER'# 1 for WINTER, 2 for SPRING, 3 for SUMMER, 4 for AUTUMN
off = 'THEFT/OTHER'
n = 1:nrow(rf_data)
trainInd = n[rf_data$OFFENSE %in% off & rf_data$YEAR %in% yrs_training]
yTestInd = n[rf_data$OFFENSE %in% off & rf_data$YEAR %in% yr_test & rf_data[,season] == 1]
m = median(rf_data$Count[trainInd])
rf_data$LABEL = ifelse(rf_data$Count <= m, -1, 1)
X = as.matrix(rf_data[,-c(1,2,3,4,5, ncol(rf_data))])
y = as.matrix(rf_data$LABEL)
#training data 
XTrain = X[trainInd,]
#input data for prediction, aka demographics etc for 2014
XTest = X[yTestInd,]
#training data labels (high or low)
yTrain <- as.factor(y[trainInd]) # = y[trainInd]
#test data labels to see how well the model performed (y is a vector)
yTest <- as.factor(y[yTestInd]) # = y[yTestInd]
#yTestNum <- y[yTestInd]


library(randomForest)
ntree = 200
mtry = 2
modelClass <- randomForest(
  x = XTrain,
  y = as.factor(yTrain),
  ntree=ntree,
  mtry=mtry,
  importance=TRUE
)

predClass <- predict(modelClass, XTest)
predClass<-as.matrix(as.numeric(predClass))
yTest<-as.matrix(as.numeric(yTest)) # convert y back to a number to calculate error (yTest?)
predErr <- length(which((predClass-yTest)>0))/length(predClass)
predErr

####plot results####
red <- "#B70024"
blue <- "#2B4A6F"
grey <- "#cccccc"

#Real Data
cols2 <-c(blue, red)
brks2<- c(1,2)
lbls2<- findInterval(yTest, brks2)
plot(dc, col=cols2[lbls2], lwd = 0.5)
title(main = paste0('Real Classification: ', off), sub = paste0('Washington DC, ', season, ' ', yr_test), 
      cex.main = 2, cex.sub = 1.5)
legend('bottomright', legend = c('Below Median', 'Above Median'), fill = cols2, bty="n", cex = 1, title = '')
#png(paste0('../FIGURES', gsub('/', '-', off), '_', season, yr_test,'.png'),
 #   width = 1200, height = 1000)

#Prediction
cols2 <-c(blue, red)
brks2<- c(1,2)
lbls3<- findInterval(predClass, brks2)
plot(dc, col=cols2[lbls3], lwd = 0.5)
title(main = paste0('RF Classification Results: ', off), sub = paste0('Washington DC, ', season, ' ', yr_test), 
      cex.main = 2, cex.sub = 1.5)
legend('bottomright', legend = c('Below Median', 'Above Median'), fill = cols2, bty="n", cex = 1, title = '')
text(x = -76.90461, y = 38.985, labels = paste('ntree = ', ntree), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.97, labels = paste('mtry = ', mtry), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.955, labels = paste('Error = ', round(predErr, 3)), cex = 1.5, pos = 4)
#png(paste0('../FIGURES', gsub('/', '-', off), '_', season, yr_test, '_ntree', as.character(ntree), '_', mtry, '.png'),
   # width = 1200, height = 1000)


#Errors
predRes <- yTest-predClass
brks1 <- c(-1,0,1)
lbls <- findInterval(predRes, brks1)
#cols <- c("blue", "grey", "red")
cols <- c(blue, grey, red)
plot(dc, col=cols[lbls], lwd = .5)
title(main = paste0('RF Classification Errors: ', off), sub = paste0('Washington DC, ', season, ' ', yr_test), 
      cex.main = 2, cex.sub = 1.5)
legend('bottomright', legend = c('Low as High ', 'Correct', 'High as Low'), fill = cols, bty="n", cex = 1, title = '')
text(x = -76.90461, y = 38.985, labels = paste('ntree = ', ntree), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.97, labels = paste('mtry = ', mtry), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.955, labels = paste('Error = ', round(predErr, 3)), cex = 1.5, pos = 4)
#text(x = -76.90461, y = 38.93, labels = paste('Error = ', round(predErr, 3)), cex = 1.5, pos = 4)



varImpPlot(modelClass)
