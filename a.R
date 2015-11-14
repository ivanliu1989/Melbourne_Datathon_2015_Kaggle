setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia)
load('../S9_train_validation_test_20151110.RData');ls()

id <- total[, 1:2]
flag <- total[, 77:78]
total <- total[,3:(ncol(total)-2)]

# Zero variance
nzv <- nearZeroVar(total, saveMetrics= TRUE)
nzv[nzv$nzv,]
nzv <- nearZeroVar(total)
total <- total[, -nzv]
dim(total)


# Correlation
descrCor <- cor(total)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
names(total[,highlyCorDescr])
total <- total[,-highlyCorDescr]
descrCor2 <- cor(total)
summary(descrCor2[upper.tri(descrCor2)])


# Linear dependency
comboInfo <- findLinearCombos(total)
comboInfo
head(total[,comboInfo$remove])
total <- total[,-comboInfo$remove]

# split
total <- cbind(id, total ,flag)
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
