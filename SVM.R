library(kernlab)
load('./data/final_sp.RData')

# SVM
yrs_training = c(2011, 2012, 2013)
yr_test = 2014
season_test = 2 ; season = 'SUMMER'# 1 for WINTER, 2 for SPRING, 3 for SUMMER, 4 for AUTUMN
off = 'BURGLARY'
dc@data = dc@data[complete.cases(dc@data),]
n = 1:nrow(dc@data)
trainInd = n[dc@data$OFFENSE %in% off & dc@data$YEAR %in% yrs_training]
yTestInd = n[dc@data$OFFENSE %in% off & dc@data$YEAR %in% yr_test & dc@data[,season] == 1]
m = median(dc@data$Count[trainInd])
dc@data$LABEL = ifelse(dc@data$Count <= m, -1, 1)
X = as.matrix(dc@data[,-c(1,2,3,4,5, ncol(dc@data))])
y = as.matrix(dc@data$LABEL)
XTrain = X[trainInd,]
XTest = X[yTestInd,]
yTrain = y[trainInd]
yTest = y[yTestInd]


# SVM test 1
svm_type = "C-svc"
c_value = 10
ker = 'rbfdot' # polydot rbfdot
kp = 'automatic'
lsModel <- ksvm(x=XTrain, y=yTrain,type=svm_type, kernel = ker, kpar = kp, C = c_value)
lsPred <- predict(lsModel, XTest)
predErr <- length(which((lsPred-yTest)>0))/length(lsPred)

pltClass <- lsModel@fitted
pltClass[lsModel@SVindex] <- 2
brks <- c(-1,1,2)
lbls <- findInterval(pltClass, brks)
col_palette <- c("orangered", "grey", "plum1")
png(paste0('./Figures/SVM_', gsub('/', '-', off), '_', season, yr_test, '_C', as.character(c_value), '_', ker, '_', kp, '.png'),
           width = 1200, height = 1000)
plot(dc[-yTestInd,], col=col_palette[lbls], lwd = .5)
title(main = paste0('SVM Classification results: ', off), sub = paste0('Washington DC, ', season, ' ', yr_test), 
      cex.main = 2, cex.sub = 1.5)
legend('bottomright', legend = c('Above median', 'Support Vectors','Below median'), fill = col_palette, bty="n", cex = 1.5, title = '')
text(x = -76.90461, y = 38.985, labels = paste('C = ', c_value), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.97, labels = paste('Kernel = ', ker), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.955, labels = paste('Kpar = ', kp), cex = 1.5, pos = 4)
text(x = -76.90461, y = 38.93, labels = paste('Error = ', round(predErr, 3)), cex = 1.5, pos = 4)
dev.off()

