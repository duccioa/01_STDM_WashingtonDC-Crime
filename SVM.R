load('./data/final_sp.RData')

# SVM
yrs_training = c(2011, 2012, 2013)
yr_test = 2014
season_test = 1 ; season = 'WINTER'# 1 for WINTER, 2 for SPRING, 3 for SUMMER, 4 for AUTUMN
off = 'THEFT/OTHER'
dc@data = dc@data[complete.cases(dc@data),]
n = 1:nrow(dc@data)
trainInd = n[dc@data$OFFENSE %in% off & dc@data$YEAR %in% yrs_training]
yTestInd = n[dc@data$OFFENSE %in% off & dc@data$YEAR %in% yr_test & dc@data[,season] == 1]
m = median(dc@data$Count[trainInd])
dc@data$LABEL = ifelse(dc@data$Count <= m, -1, 1)
X = as.matrix(dc@data[,-c(1,2,3,4,5, ncol(dc@data))])
y = as.matrix(dc@data$LABEL)
XTrain = X[trainInd,]
XTest = X[-trainInd,]; XTest = XTest[XTest[,season_test] == 1,]
yTrain = y[trainInd]
yTest = y[yTestInd]


# SVM test 1
svm_type = "C-svc"
c_value = 1
kernel = 'rbfdot'
lsModel <- ksvm(x=XTrain, y=yTrain,type=svm_type)
lsPred <- predict(lsModel, XTest)




pltClass <- lsModel@fitted
pltClass[lsModel@SVindex] <- 2
brks <- c(-1,1,2)
lbls <- findInterval(pltClass, brks)
col_palette <- c("plum1", "grey", "orangered")
plot(dc[-trainInd,], col=cols[lbls])
title(main = paste0('SVM Classification results: ', off), sub = paste0('Washington DC, ', season), 
      cex.main = 1, cex.sub = .8)
legend('bottomright', legend = c('Above median', 'Support Vectors','Below median'), fill = col_palette, bty="n", cex = .8, title = '')




