source('./FUN_runSVM.R')


param_df = data.frame(Configuration = c('Std', 'A', 'B', 'C','A_opt','D'),
                   Type = c('C-svc', 'C-svc', 'C-svc', 'C-svc','C-svc', 'C-svc'),
                   C_value = c(10, 1, 100, 100,1, 1),
                   Kernel = c('rbfdot', 'rbfdot','rbfdot','rbfdot','rbfdot','polydot'),
                   Cross_val = c(0,0,0,10,0,3), 
                   Error = NA,
                   BelMed = NA,
                   AbvMed = NA,
                   SVs = NA)
conf = 'Std'
param = param_df[param_df$Configuration %in% conf,]
Std0 = run.SVM(svm_type = param[1,2],
              c_value = param[1,3],
              ker = param[1,4], # rbfdot
              kp = 'automatic',
              cross_val = param[1,5],
              yrs_tr = c(2011),
              config = conf)

conf = 'Std'
param = param_df[param_df$Configuration %in% conf,]
Std = run.SVM(svm_type = param[1,2],
              c_value = param[1,3],
              ker = param[1,4], # rbfdot
              kp = 'automatic',
              cross_val = param[1,5],
              yrs_tr = c(2011, 2012, 2013),
              config = conf)
param_df[param_df$Configuration %in% conf,6:9] = Std

conf = 'A'
param = param_df[param_df$Configuration %in% conf,]
A = run.SVM(svm_type = param[1,2],
              c_value = param[1,3],
              ker = param[1,4], # rbfdot
              kp = 'automatic',
              cross_val = param[1,5],
              yrs_tr = c(2011, 2012, 2013),
              config = conf)
param_df[param_df$Configuration %in% conf,6:9] = A

conf = 'B'
param = param_df[param_df$Configuration %in% conf,]
B = run.SVM(svm_type = param[1,2],
              c_value = param[1,3],
              ker = param[1,4], # rbfdot
              kp = 'automatic',
              cross_val = param[1,5],
              yrs_tr = c(2011, 2012, 2013),
              config = conf)
param_df[param_df$Configuration %in% conf,6:9] = B

conf = 'C'
param = param_df[param_df$Configuration %in% conf,]
C = run.SVM(svm_type = param[1,2],
              c_value = param[1,3],
              ker = param[1,4], # rbfdot
              kp = 'automatic',
              cross_val = param[1,5],
              yrs_tr = c(2011, 2012, 2013),
              config = conf)
param_df[param_df$Configuration %in% conf,6:9] = C

conf = 'A_opt'
param = param_df[param_df$Configuration %in% conf,]
A_opt = run.SVM(svm_type = param[1,2],
            c_value = param[1,3],
            ker = param[1,4], # rbfdot
            kp = 'automatic',
            cross_val = param[1,5],
            yrs_tr = c(2011, 2012, 2013),
            config = conf,
            optimal = T)
param_df[param_df$Configuration %in% conf,6:9] = A_opt


conf = 'D'
param = param_df[param_df$Configuration %in% conf,]
D = run.SVM(svm_type = param[1,2],
              c_value = param[1,3],
              ker = param[1,4], # polydot
              kp = 'automatic',
              cross_val = param[1,5],
              yrs_tr = c(2011, 2012, 2013),
              config = conf)
param_df[param_df$Configuration %in% conf,6:9] = D



