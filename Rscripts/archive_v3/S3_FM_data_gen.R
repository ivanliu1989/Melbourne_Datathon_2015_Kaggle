setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list = ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_20151106.RData');ls()