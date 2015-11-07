setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list = ls()); gc()
library(RSofia);library(pROC);
load('data/9_train_validation_test_20151108.RData');ls()

train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
feat <- c(3:61)

# parse
dt <- parse_formula(flag_class ~ ., train[,feat])


# predict
p <- predict(model.logreg, newdata=irismod[-1*i.TRAIN,], prediction_type = "logistic")