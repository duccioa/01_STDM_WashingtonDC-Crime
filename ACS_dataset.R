source('STDM_functions.R')
folder = './data/ACS/'
acs_years = 12:14
acs_table = c('DP02', 'DP03', 'DP04', 'DP05')

acs_dp02_14 = load_ACS('DP02', 14)
sel20 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel21 = paste0('HC01_VC', formatC(c(3:14,76, 86:92), width = 2, flag=0))
sel22 = paste0('HC03_VC', formatC(c(3:14,76, 86:92), width = 2, flag=0))
dp02_14 = subset(acs_dp02_14, select = c(sel20,sel21, sel22)); rm(acs_dp02_14, sel20,sel21,sel22)

acs_dp03_14 = load_ACS('DP03', 14)
sel30 = c('GEO.id', 'GEO.id2', 'GEO.display.label')
sel31 = paste0('HC01_VC', formatC(c(3:09,12,75:84,124, 171), width = 2, flag=0))
sel32 = paste0('HC03_VC', formatC(c(3:09,12,75:84,124, 171), width = 2, flag=0))
dp03_14 = subset(acs_dp03_14, select = c(sel30,sel31, sel32)); rm(acs_dp03_14, sel30,sel31,sel32)

acs_dp04_14 = load_ACS('DP04', 14)
sel40 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel41 = paste0('HC01_VC', formatC(c(5,65,119:126, 127,197:202), width = 2, flag=0))
sel42 = paste0('HC03_VC', formatC(c(5,65,119:126, 127,197:202), width = 2, flag=0))
dp04_14 = subset(acs_dp04_14, select = c(sel40,sel41, sel42)); rm(acs_dp04_14, sel40,sel41,sel42)

acs_dp05_14 = load_ACS('DP05', 14)
sel50 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel51 = paste0('HC01_VC', formatC(c(8:20,23), width = 2, flag=0))
sel52 = paste0('HC03_VC', formatC(c(8:20,23), width = 2, flag=0))
dp05_14 = subset(acs_dp05_14, select = c(sel50,sel51, sel52)); rm(acs_dp05_14, sel50,sel51,sel52)



