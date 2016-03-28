test.SVM = function(svm_type = 'C-svc',
                    c_value = c(10,100,1000),
                    sigma_value = c(0.001, 0.01, 0.1),
                    met = "svmRadial", #svmPoly
                    yrs_tr = c(2011, 2012, 2013),
                    cross_n = 5,
                    optimal = F, 
                    config = 'lsM1'){# removes variables that are not meaninful, selected with random forest
    
    
    
    require(sp)
    require(rgdal)
    require(kernlab)
    require(caret)
    if(!exists('census')){
        census = read.csv('./data/ACS/census_fullset_for_ML.csv', stringsAsFactors = F)
        census = census[,-1]
        census$CENSUS_TRACT = formatC(census$CENSUS_TRACT, width = 6, flag = 00)
        census$YEAR = as.character(census$YEAR)
        census <<- census
    }
    
    if(!exists('dc')){
        dc = readOGR('./shp/cb_2014_11_tract_500k/', 'cb_2014_11_tract_500k')
        names(dc@data)[3] = 'CENSUS_TRACT'
        dc@data = dc@data[,-c(2,4,5,6,7,8, 9)]
        dc <<- dc
    }
    
    #### Prepare for SVM ####
    svm_data = read.csv('./data/aggr1.csv')
    yrs_training = yrs_tr# Training with single or multiple years
    yr_test = 2014
    TR = ifelse(length(yrs_training)>1, 'TM', 'TS')
    season_test = 3 ; season = 'SUMMER'# 1 for WINTER, 2 for SPRING, 3 for SUMMER, 4 for AUTUMN
    off = 'BURGLARY'
    n = 1:nrow(svm_data)
    trainInd = n[svm_data$OFFENSE %in% off & svm_data$YEAR %in% yrs_training]
    yTestInd = n[svm_data$OFFENSE %in% off & svm_data$YEAR %in% yr_test & svm_data[,season] == 1]
    m = median(svm_data$Count[trainInd])
    svm_data$LABEL = ifelse(svm_data$Count <= m, -1, 1)
    X = as.matrix(svm_data[,-c(1,2,3,4,5, ncol(svm_data))])
    if(optimal){
        X = X[,c(1:4, 11,12,16,52)]
        config = paste0(config,'opt')}
    y = as.matrix(svm_data$LABEL)
    XTrain = X[trainInd,]
    XTest = X[yTestInd,]
    yTrain = y[trainInd]
    yTest = y[yTestInd]
    
    
    
    scaling = as.logical(c(rep(0,4), rep(1, ncol(XTrain)-4)))
    Xtrain = cbind(XTrain[,1:4],scale(XTrain[,scaling]))
    if(met == 'svmPoly'){
        ctrl = trainControl(method = "cv", number=cross_n)
        SVMGrid = expand.grid(.degree = c(2,3), .scale = c(1,5,10), .C = c_value)
    }
    else{
    ctrl = trainControl(method = "cv", number=cross_n)
    SVMGrid = expand.grid(.sigma=sigma_value, .C=c_value)
    }
    SVMFit = train(XTrain, as.factor(yTrain), method=met, tuneGrid=SVMGrid,
                   trControl=ctrl, type="C-svc")
    output = list()
    output$Model = SVMFit
    output$FinalModel = SVMFit$finalModel
    output$Plot = plot(SVMFit)
    
    output$PredXTtest = predict(SVMFit, XTest)
    output$PredErrXTest = length(which(output$PredXTtest!=yTest))/length(output$PredXTtest)
    
    
    return(output)
}
