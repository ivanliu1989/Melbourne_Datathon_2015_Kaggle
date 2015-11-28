setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
load('data/9_train_validation_test_20151122.RData');ls()
options(scipen=999);set.seed(19890624)

head(train)

ffmlib_convert <- function(dt){
    dt <- dt[,c(58, 3:57)]
    dt$flag_class <- ifelse(dt$flag_class == 'Y', 1, 0)
    for(i in 2:ncol(dt)){
        print(i)
        if(i==2){
            dt$ffm <- paste0(dt$flag_class,' ',i-1,':', i-1,':',dt[,i])
        }else{
            dt$ffm <- paste0(dt$ffm,' ',i-1,':', i-1,':',dt[,i])
        }   
    }
    return(dt$ffm)
}

train_ffm <- ffmlib_convert(train)
total_ffm <- ffmlib_convert(total)
test_ffm <- ffmlib_convert(test)
valid_ffm <- ffmlib_convert(validation)

write.table(train_ffm, file='../train_ffm.txt', quote = F, row.names = F, col.names = F)
write.table(total_ffm, file='../total_ffm.txt', quote = F, row.names = F, col.names = F)
write.table(test_ffm, file='../test_ffm.txt', quote = F, row.names = F, col.names = F)
write.table(valid_ffm, file='../valid_ffm.txt', quote = F, row.names = F, col.names = F)


svmlib_convert <- function(dt){
    dt <- dt[,c(58, 3:57)]
    dt$flag_class <- ifelse(dt$flag_class == 'Y', 1, 0)
    for(i in 2:ncol(dt)){
        print(i)
        if(i==2){
            dt$ffm <- paste0(dt$flag_class,' ',i-1,':',dt[,i])
        }else{
            dt$ffm <- paste0(dt$ffm,' ',i-1,':',dt[,i])
        }   
    }
    return(dt$ffm)
}

train_svm <- svmlib_convert(train)
total_svm <- svmlib_convert(total)
test_svm <- svmlib_convert(test)
valid_svm <- svmlib_convert(validation)

write.table(train_svm, file='../train_svm.txt', quote = F, row.names = F, col.names = F)
write.table(total_svm, file='../total_svm.txt', quote = F, row.names = F, col.names = F)
write.table(test_svm, file='../test_svm.txt', quote = F, row.names = F, col.names = F)
write.table(valid_svm, file='../valid_svm.txt', quote = F, row.names = F, col.names = F)
