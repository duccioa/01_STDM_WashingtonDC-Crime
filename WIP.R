library(lubridate)
library(data.table)
library(ggplot2)
library(kernlab)
library(reshape2)
source('STDM_functions.R')
####LOAD CENSUS
#df = read.csv('./data/FINAL_CrimeCensus_merged.csv')
#df = df[,-c(1, 11, 12, 13)]
#df$START_DATE = ymd_hms(df$START_DATE)
#df$YEAR = as.factor(df$YEAR)
#df$WEEK = as.factor(df$WEEK)
#df$MONTH = as.factor(df$MONTH)
#summary(df)
#save(df, file = './data/CrimeCensus_merged.RData')
load('./data/CrimeCensus_merged.RData')
census = read.csv('./data/ACS/census_fullset_for_ML.csv')
census = census[,-c(1:4)]
census = data.table(census)
df = data.table(df)
unempl = df[OFFENSE == 'BURGLARY',.(Avg_Unempl = mean(DP03_PCT_VC13, na.rm = T), 
                                    Avg_PovRate = mean(DP03_PCT_VC166, na.rm = T),
                                    CrimeCount = .N), by = YEAR]
unempl[,YEAR := as.character(YEAR)]
unempl[,Avg_Unempl := scale(Avg_Unempl)]
unempl[,CrimeCount:=scale(CrimeCount)]


attach(unempl)
plot(YEAR, CrimeCount, col = 'red', type = 'l')
lines(YEAR, Avg_Unempl, col = 'green')
detach(unempl)


unempl = data.frame(unempl)
unempl = data.frame(cbind(as.character(unempl$YEAR), scale(unempl[,2:4])))

unempl1 = unempl[,c(1,2,4)]
unempl1 = melt(unempl1, value.name = 'Avg_Unempl')
g = ggplot(unempl, aes(x = V1, y = Avg_Unempl, group = 1)) + geom_line(color = 'green') + 
    geom_line(aes(y = CrimeCount), color = 'red')
g = g + ggtitle('Average Poverty Rate and Crime Rate') + xlab('Washington DC, Year') + ylab('Normalised values') +
    theme(axis.text.y = element_blank()) + 
g



