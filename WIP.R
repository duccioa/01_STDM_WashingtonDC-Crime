library(lubridate)
library(data.table)
library(ggplot2)
library(kernlab)
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
census$CENSUS_TRACT = as.factor(formatC(census$CENSUS_TRACT, width = 6, flag = 00))
df = data.table(df)
df$SEASON = df[,.(SEASON = ifelse(df$MONTH %in% c(9,10,11), 'AUTUMN', ifelse(df$MONTH %in% c(6,7,8), 'SUMMER', ifelse(df$MONTH %in% c(3,4,5), 'SPRING', 'WINTER'))))]
# Classification
aggr1 = df[, .(Count = .N), by = .(OFFENSE, CENSUS_TRACT, SEASON, YEAR)]
aggr1 = aggr1[OFFENSE == 'THEFT/OTHER']
hist(aggr1$Count, breaks = 176)
abline(v = mean(aggr1$Count), col = 'red')
abline(v = median(aggr1$Count), col = 'blue')
abline(v = getmode(aggr1$Count), col = 'cyan', lwd = 2)
m = median(aggr1$Count)
aggr1$LABEL = aggr1[,.(LABEL = ifelse(aggr1$Count <= m, -1, 1))]
#aggr1$LABEL = as.factor(aggr1$LABEL)
aggr1$WINTER = aggr1[,.(WINTER = ifelse(aggr1$SEASON == 'WINTER', 1, 0))]
aggr1$SPRING = aggr1[,.(SPRING = ifelse(aggr1$SEASON == 'SPRING', 1, 0))]
aggr1$SUMMER = aggr1[,.(SUMMER = ifelse(aggr1$SEASON == 'SUMMER', 1, 0))]
aggr1$AUTUMN = aggr1[,.(AUTUMN = ifelse(aggr1$SEASON == 'AUTUMN', 1, 0))]

#SVM
aggr2 = aggr1[YEAR %in% c('2011', '2012', '2013')]

aggr_training = data.frame(aggr2)
Xtraining = join(aggr_training, census)
Xtraining = Xtraining[,-c(1,2,3,4,5,6)]
Ytraining = as.character(aggr2$LABEL)

aggr3 = aggr1[YEAR %in% '2014']
aggr_test = data.frame(aggr3)
Xtest = join(aggr3, census)
Xtest = Xtest[,-c(1,2,3,4,5,6)]
Ytest = as.character(aggr3$LABEL)

modelClass <- ksvm(
    x = as.matrix(Xtraining),
    y = as.matrix(Ytraining),
    scaled=TRUE,
    type="C-svc",
    kernel="rbfdot",
    kpar="automatic",
    C=1,
    cross=0
)

pltClass <- modelClass@fitted
pltClass[modelClass@SVindex] <- 2
brks <- c(-1,1,2)
lbls <- findInterval(pltClass, brks)
cols <- c("blue", "red", "black")
plot