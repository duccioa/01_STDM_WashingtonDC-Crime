source('./FUN_SVM_Caret.R')
source('./FUN_runSVM.R')  
lsM0 = test.SVM(yrs_tr = 2011) # test with one year only training
lsM01 = test.SVM(yrs_tr = 2012) # test with one year only training
lsM02 = test.SVM(yrs_tr = 2013)
lsM1 = test.SVM() # test with multiple years training
lsM2 = test.SVM(sigma_value = c(0.0005, 0.001, 0.0015), c_value = c(1,10,50)) # Parameter refinement
lsM3 = test.SVM(sigma_value = c(0.0005, 0.001, 0.0015), c_value = c(1,10,50), opt = T, config = 'lsM3')
lsM4 = test.SVM(sigma_value = c(0.0005, 0.001, 0.0015), c_value = c(1,10,50), met = 'svmPoly', config = 'lsM4')
    

