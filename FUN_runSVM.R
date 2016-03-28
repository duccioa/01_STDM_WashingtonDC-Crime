
run.SVM = function(svm_type = 'C-svc',
                   c_value = 1,
                   ker = 'rbfdot', # polydot rbfdot
                   kp = 'automatic',
                   cross_val = 0,
                   yrs_tr = c(2011, 2012, 2013), 
                   config = 'Std',
                   optimal = F){# removes variables that are not meaninful, selected with random forest
    require(sp)
    require(rgdal)
    require(kernlab)
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
    
    
    ##### SVM test 1 ####
    
    scaling = as.logical(c(rep(0,4), rep(1, ncol(XTrain)-4)))
    lsModel = ksvm(x=XTrain, y=yTrain,
                   type=as.character(svm_type), 
                   kernel = as.character(ker), 
                   kpar = kp, 
                   C = as.numeric(c_value), 
                   cross = as.numeric(cross_val), 
                   scaled = scaling) 
    lsPred = predict(lsModel, XTest)
    predErr = length(which(lsPred!=yTest))/length(lsPred)
    #plot test data against results
    red = '#B70024'
    blue = '#2B4A6F'
    bluegrey = 'darkslategray4'
    orange = 'orange'
    green = 'green'
    
    
    png(paste0('./Figures/SVM_',config,'_', gsub('/', '-', off), '_', TR, '_',season, yr_test, '_C', as.character(c_value), '_', ker, '_', kp, '.png'),
        width = 1200, height = 1000)
    par(mfrow = c(1,2), mar = c(5,1,13,1))
    col_pal_test = c(blue,red)
    pltClass_test = yTest
    brks_test = c(-1,1)
    lbls_test = findInterval(pltClass_test, brks_test)
    
    col_pal_res = c(blue,red, bluegrey)
    pltClass_res = lsPred
    pltClass_res[lsPred!=yTest] = 2
    brks_res = c(-1,1,2)
    lbls_res = findInterval(pltClass_res, brks_res)
    
    plot(dc, col = col_pal_test[lbls_test], lwd = .5) 
    title(main = 'Real Data', cex.main = 2)
    legend('bottomleft', inset = c(0,0.05),legend = c('Below median', 'Above Median','Prediction Errors'), 
           fill = col_pal_res, bty='n', cex = 1.3, title = '')
    plot(dc, col = col_pal_res[lbls_res], lwd = .5)
    title(main = 'SVM prediction', cex.main = 2)
    mtext('SVM Classification', side = 3, outer = T, line = -2, cex = 2.5, font = 2)
    mtext(off, side = 3, outer = T, line = -4.5, cex = 2, font = 2)
    mtext(paste0('Washington DC, ', season, ' ', yr_test), side = 1, line = -1.5, outer = T, cex = 1.5)
    text(x = -77.12, y = 38.783, labels = paste('Training yrs = ', 
                                                ifelse(TR == 'TS', '2011', '2011-2012-2013')), cex = 1.3, pos = 4)
    text(x = -77.12, y = 38.823, labels = paste('C = ', c_value), cex = 1.3, pos = 4)
    text(x = -77.12, y = 38.813, labels = paste('Kernel = ', ker), cex = 1.3, pos = 4)
    text(x = -77.12, y = 38.803, labels = paste('Kpar = ', kp), cex = 1.3, pos = 4)
    text(x = -77.12, y = 38.793, labels = paste('Error = ', round(predErr, 3)), cex = 1.3, pos = 4)
    dev.off()
    
    
    #plot barplots
    par(mfrow = c(1,2))
    tb_test = table(pltClass_test)
    names(tb_test) = c('Below Median', 'Above Median')
    png(paste0('./Figures/SVM_',config,'_', gsub('/', '-', off), '_', TR, '_',season, yr_test, '_C', as.character(c_value), '_', ker, '_', kp, '_BAR.png'),
        width = 1200, height = 500)
    tb_pre = table(pltClass_res)
    names(tb_pre) = c('Below Median', 'Above Median','SVs')
    par(mfrow = c(1,2))
    barplot(tb_test,col=c(blue,red),
            legend = NULL, beside=TRUE, border = 'white', cex.names = 1.5)
    barplot(tb_pre,col=c(blue, red, orange),
            legend = NULL, beside=TRUE, border = 'white', cex.names = 1.5)
    legend('topright', rownames(tb_pre),fill = c(blue, red, orange), bty = 'n', cex = 1.5)
    dev.off()
    output = round(c(predErr, tb_pre[1], tb_pre[2], tb_pre[3]),3)
    names(output) = c('Error', 'Below Median', 'Above Median', 'SVs')
    
    
    
    
    return(output)
}