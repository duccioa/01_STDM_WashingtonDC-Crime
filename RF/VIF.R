setwd("/Users/sarahhank/Dropbox/CASA/STDM/Coursework/DATA")
library("usdm")
census<-read.csv("census_fullset_for_ML.csv")
cencrime<-read.csv("FINAL_CrimeCensus_merged.csv")
#xtraining<-read.csv("xtraining.csv")

vifcensus<-vif(census)

write.csv(vifcensus, file="vifcensus.csv")

#Principal Component Analysis
library("stats")
#refers to X matrix from SVM.R or Random_Forest_STDM.R
X2 = as.matrix(rf_data[,-c(1,2,3,4,5)])
XTrain2 = X2[trainInd,]

# log transform 
#log.XTrain2 <- log(XTrain2)
#crime <- log.XTrain2
crime <- XTrain2[,1:61]
crime.label <- XTrain2[, 62]
#ncol(XTrain2)
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
crime.pca <- prcomp(crime,
                 center = TRUE,
                 scale. = TRUE) 
print(crime.pca)

# plot method
plot(crime.pca, type = "l")
summary(crime.pca)

newdat<-crime.pca$x
