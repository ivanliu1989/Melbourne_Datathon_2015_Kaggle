setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("mxnet")
library(mlbench);library(pROC);require(mxnet);library(caret);
load('data/9_train_validation_test_20151122.RData');ls()


data(Sonar, package="mlbench")

Sonar[,61] = as.numeric(Sonar[,61])-1
train.ind = c(1:50, 100:150)
train.x = data.matrix(Sonar[train.ind, 1:60])
train.y = Sonar[train.ind, 61]
test.x = data.matrix(Sonar[-train.ind, 1:60])
test.y = Sonar[-train.ind, 61]

mx.set.seed(0)
model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                eval.metric=mx.metric.accuracy)