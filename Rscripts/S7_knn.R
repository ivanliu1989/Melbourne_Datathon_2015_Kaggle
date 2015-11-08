setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(pROC);library(caret)
load('data/9_train_validation_test_20151108.RData');ls()


train$flag_class <- as.factor(train$flag_class)
feat <- c(3:61)
fitControl <- trainControl(method = "none",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
Grid <-  expand.grid(k=10) # 2, 4, 6, 8, 16, 32, 64, 128, 256, 512, 1024 
fit <- train(flag_class ~ ., data=train[,feat], # classification
             method = "knn",
             trControl = fitControl,
             tuneGrid = Grid,
             preProcess = c('center', 'scale'),
             metric ='ROC',
             verbose = T)

### predict
val <- validation
p <- predict(fit, newdata=val, type = 'prob')
total$caret_knn2_meta <- as.vector(p)