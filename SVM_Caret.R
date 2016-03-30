source('./FUN_SVM_Caret.R')
source('./FUN_runSVM.R')  
lsM0 = test.SVM(yrs_tr = 2011) # test with one year only training
lsM01 = test.SVM(yrs_tr = 2012) # test with one year only training
lsM02 = test.SVM(yrs_tr = 2013)
lsM1 = test.SVM(sigma_value = c(0.001, 0.01, 0.1), c_value = c(10,100,1000), cross_n = 5) # test with multiple years training

lsM2 = test.SVM(sigma_value = c(0.0005, 0.001, 0.0015), c_value = c(50,100,150), cross_n = 5)
lsM21 = test.SVM(sigma_value = c(0.0012, 0.0015, 0.0018), c_value = c(80, 100, 120), cross_n = 5)# Parameter refinement
lsM22 = test.SVM(sigma_value = c(0.0011, 0.0012, 0.0013), c_value = c(95, 100, 105), cross_n = 10)




lsM3 = test.SVM(sigma_value = c(0.0011, 0.0012, 0.0013), c_value = c(95, 100, 105), opt = T, config = 'lsM3') # test optimal variables
lsM31 = test.SVM(sigma_value = c(0.0001, 0.0011, 0.0020), c_value = c(20,50,90), opt = T, config = 'lsM3') # test optimal variables
lsM32 = test.SVM(sigma_value = c(0.00001, 0.0001, 0.0002), c_value = c(1,10,20), opt = T, config = 'lsM3') # test optimal variables



lsM4 = {ctrl = trainControl(method = "cv", number=5)
SVMGrid = expand.grid(.degree = c(0.5,1, 2), .scale = c(0.005,0.01,.05), .C = c_value)
SVMFit = train(XTrain, as.factor(yTrain), method=met, tuneGrid=SVMGrid,
               trControl=ctrl, type="C-svc")}
    

# 
conf = 'lsM22'
lsM22_ksvm = run.SVM(svm_type = 'C-svc',
                    c_value = 105,
                    ker = 'rbfdot', # polydot
                    kp = list(sigma = 0.0013),
                    cross_val = 0,
                    yrs_tr = c(2011, 2012, 2013),
                    config = conf)
conf = 'lsM22a'
lsM22a_ksvm = run.SVM(svm_type = 'C-svc',
                     c_value = 50,
                     ker = 'rbfdot', # polydot
                     kp = list(sigma = 0.0013),
                     cross_val = 0,
                     yrs_tr = c(2011, 2012, 2013),
                     config = conf)
conf = 'lsM22b'
lsM22a_ksvm = run.SVM(svm_type = 'C-svc',
                      c_value = 200,
                      ker = 'rbfdot', # polydot
                      kp = list(sigma = 0.0013),
                      cross_val = 0,
                      yrs_tr = c(2011, 2012, 2013),
                      config = conf)

## 
conf = 'lsM4'
lsM4_ksvm = run.SVM(svm_type = 'C-svc',
                    c_value = 105,
                    ker = 'polydot', # polydot
                    kp = 'automatic',
                    cross_val = 0,
                    yrs_tr = c(2011, 2012, 2013),
                    config = conf)



