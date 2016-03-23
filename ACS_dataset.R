library(plyr)
source('STDM_functions.R')
folder = './data/ACS/'
acs_years = 11:14
acs_table = c('DP02', 'DP03', 'DP04', 'DP05')


#DP02
acs_dp02_11 = load_ACS('DP02', 11)
acs_dp02_12 = load_ACS('DP02', 12)
sel20a = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel21a = paste0('HC01_VC', formatC(c(3:4,6:15,75, 85:91), width = 2, flag=0))
#sel22a = paste0('HC03_VC', formatC(c(3:4,6:15,75, 85:91), width = 2, flag=0))
acs_dp02_11 = subset(acs_dp02_11, select = c(sel20a,sel21a))
acs_dp02_12 = subset(acs_dp02_12, select = c(sel20a,sel21a))
tmp1 = rbind(acs_dp02_11, acs_dp02_12)

acs_dp02_13 = load_ACS('DP02', 13)
acs_dp02_14 = load_ACS('DP02', 14)
sel20 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel21 = paste0('HC01_VC', formatC(c(3:14,76, 86:92), width = 2, flag=0))
#sel22 = paste0('HC03_VC', formatC(c(3:14,76, 86:92), width = 2, flag=0))
acs_dp02_13 = subset(acs_dp02_13, select = c(sel20,sel21))
acs_dp02_14 = subset(acs_dp02_14, select = c(sel20,sel21))
tmp2 = rbind(acs_dp02_13, acs_dp02_14)
names(tmp2)[5:ncol(tmp2)] = names(tmp1)[5:ncol(tmp1)]

acs_dp02 = rbind(tmp1, tmp2)
#for(i in 1:4){acs_dp02[,i] = as.factor(acs_dp02[,i])}
rm(acs_dp02_11, acs_dp02_12, acs_dp02_13, acs_dp02_14,sel20,sel21,sel20a,sel21a,tmp1, tmp2)
write.csv(acs_dp02, './data/ACS/acs_dp02.csv')


#DP03
acs_dp03_11 = load_ACS('DP03', 11)
acs_dp03_12 = load_ACS('DP03', 12)
sel30a = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel31a = paste0('HC01_VC', formatC(c(4:10,75:85,121), width = 2, flag=0))
sel32a = paste0('HC03_VC', formatC(c(13,166), width = 2, flag=0))
acs_dp03_11 = subset(acs_dp03_11, select = c(sel30a,sel31a, sel32a))
acs_dp03_12 = subset(acs_dp03_12, select = c(sel30a,sel31a, sel32a))
tmp1 = rbind(acs_dp03_11, acs_dp03_12)


acs_dp03_13 = load_ACS('DP03', 13)
acs_dp03_14 = load_ACS('DP03', 14)
sel30 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel31 = paste0('HC01_VC', formatC(c(3:9,75:85,124), width = 2, flag=0))
sel32 = paste0('HC03_VC', formatC(c(12,171), width = 2, flag=0))
acs_dp03_13 = subset(acs_dp03_13, select = c(sel30,sel31, sel32))
acs_dp03_14 = subset(acs_dp03_14, select = c(sel30,sel31, sel32))
tmp2 = rbind(acs_dp03_13, acs_dp03_14)
names(tmp2)[5:ncol(tmp2)] = names(tmp1)[5:ncol(tmp1)]

acs_dp03 = rbind(tmp1, tmp2)
#for(i in 1:4){acs_dp03[,i] = as.factor(acs_dp03[,i])}
write.csv(acs_dp03, './data/ACS/acs_dp03.csv')
rm(acs_dp03_11, acs_dp03_12, acs_dp03_13, acs_dp03_14,sel30,sel31,sel32, sel30a,sel31a,sel32a, tmp1, tmp2)


#DP04
acs_dp04_11 = load_ACS('DP04', 11)
acs_dp04_12 = load_ACS('DP04', 12)
sel40a = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel41a = paste0('HC01_VC', formatC(c(5,64, 117:125, 192:197), width = 2, flag=0))
#sel42a = paste0('HC03_VC', formatC(c(5,64, 117:125, 192:197), width = 2, flag=0))
acs_dp04_11 = subset(acs_dp04_11, select = c(sel40a,sel41a))
acs_dp04_12 = subset(acs_dp04_12, select = c(sel40a,sel41a))
tmp1 = rbind(acs_dp04_11, acs_dp04_12)


acs_dp04_13 = load_ACS('DP04', 13)
acs_dp04_14 = load_ACS('DP04', 14)
sel40 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel41 = paste0('HC01_VC', formatC(c(5,65, 119:127, 197:202), width = 2, flag=0))
#sel42 = paste0('HC03_VC', formatC(c(5,65, 119:127, 197:202), width = 2, flag=0))
acs_dp04_13 = subset(acs_dp04_13, select = c(sel40,sel41))
acs_dp04_14 = subset(acs_dp04_14, select = c(sel40,sel41))
tmp2 = rbind(acs_dp04_13, acs_dp04_14)
names(tmp2)[5:ncol(tmp2)] = names(tmp1)[5:ncol(tmp1)]

acs_dp04 = rbind(tmp1, tmp2)
#for(i in 1:4){acs_dp04[,i] = as.factor(acs_dp04[,i])}
write.csv(acs_dp04, './data/ACS/acs_dp04.csv')
rm(acs_dp04_11, acs_dp04_12, acs_dp04_13, acs_dp04_14,sel40,sel41,sel40a,sel41a,tmp1, tmp2)


#DP05
acs_dp05_11 = load_ACS('DP05', 11)
acs_dp05_12 = load_ACS('DP05', 12)
sel50a = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel51a = paste0('HC01_VC', formatC(c(7:19, 21), width = 2, flag=0))
#sel52a = paste0('HC03_VC', formatC(c(7:19, 21), width = 2, flag=0))
acs_dp05_11 = subset(acs_dp05_11, select = c(sel50a,sel51a))
acs_dp05_12 = subset(acs_dp05_12, select = c(sel50a,sel51a))
tmp1 = rbind(acs_dp05_11, acs_dp05_12)


acs_dp05_13 = load_ACS('DP05', 13)
acs_dp05_14 = load_ACS('DP05', 14)
sel50 = c('GEO.id', 'GEO.id2', 'GEO.display.label', 'YEAR')
sel51 = paste0('HC01_VC', formatC(c(8:20, 23), width = 2, flag=0))
#sel52 = paste0('HC03_VC', formatC(c(8:20, 23), width = 2, flag=0))
acs_dp05_13 = subset(acs_dp05_13, select = c(sel50,sel51))
acs_dp05_14 = subset(acs_dp05_14, select = c(sel50,sel51))
tmp2 = rbind(acs_dp05_13, acs_dp05_14)
names(tmp2)[5:ncol(tmp2)] = names(tmp1)[5:ncol(tmp1)]

acs_dp05 = rbind(tmp1, tmp2)
#for(i in 1:4){acs_dp05[,i] = as.factor(acs_dp05[,i])}
write.csv(acs_dp05, './data/ACS/acs_dp05.csv')
rm(acs_dp05_11, acs_dp05_12, acs_dp05_13, acs_dp05_14,sel50,sel51,sel50a,sel51a,tmp1, tmp2)


#CENSUS DATASET
acs_dp02 = rename_ACS(acs_dp02, 'DP02')
acs_dp03 = rename_ACS(acs_dp03, 'DP03')
acs_dp04 = rename_ACS(acs_dp04, 'DP04')
acs_dp05 = rename_ACS(acs_dp05, 'DP05')
census = join(acs_dp02, acs_dp03)
census = join(census, acs_dp04)
census = join(census, acs_dp05)
census$CENSUS_TRACT = as.factor(rm.first_ch(census$GEO.id, 14))

write.csv(census, './data/ACS/census_fullset_for_ML.csv')






