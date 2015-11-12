setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret)
load('../S9_train_validation_test_20151110.RData');ls()
options(scipen=999)
set.seed(1004)

train <- train
test <- validation
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:(ncol(train)-2))]

y <- train$flag_class
x = rbind(train[,feat],test[,feat])
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))

# x = 1/(1+exp(-sqrt(x)))

#############################
### Raw prediction ##########
#############################
    
    # trind = 1:length(y)
    # teind = (nrow(train)+1):nrow(x)
    # 
    # trainX = x[trind,]
    # testX = x[teind,]
    
    # # Train the model
    # bst = xgboost(data = cbind(train[baggedIndex,],bagPredictions), label = as.factor(y[baggedIndex]), 
    #               max.depth = 6, eta = 0.15, nround = 500, min_child_weight = 4, colsample_bytree = 0.8,
    #               nthread = 4, objective = "binary:logistic", verbose = 0, gamma = 0.1) 
    # 
    # # Make prediction
    # pred = predict(bst,testX)
    # pred = matrix(pred,9,length(pred)/9)
    # pred = t(pred)

#################################
### Meta bagging Model ##########
#################################
testPredictions <- matrix(0, nrow = nrow(test), ncol = 1)
bootRounds = 1:240

for (j in bootRounds) {
  print(j)
  # Do the bootstrap resampling
  baggedIndex = sample(nrow(train), size = nrow(train), replace = T)
  OOBIndex = setdiff(1:nrow(train), baggedIndex)
  
  # Build the OOB model
  OOBModel = randomForest(x=train[OOBIndex,feat], y=as.factor(y[OOBIndex]), replace=F, ntree=100, do.trace=T, mtry=7)
  bagPredictions = predict(OOBModel, train[baggedIndex,], type="prob")
  bagTestPredictions = predict(OOBModel, test, type="prob")
  
  # Build the Bag model
  BagModel = xgboost(data = as.matrix(cbind(train[baggedIndex,feat],bagPredictions)), label = y[baggedIndex], 
                     max.depth = 6, eta = 0.15, nround = 500, min_child_weight = 4, colsample_bytree = 0.8,
                    nthread = 4, objective = "binary:logistic", verbose = 0, gamma = 0.1) 
  
  # Make test predictions and reshape them
  tmpPredictions = predict(BagModel,as.matrix(cbind(test,bagTestPredictions)))
  testPredictions = testPredictions + tmpPredictions
}
testPredictions = testPredictions/j


val <- validation
val$Y <- testPredictions
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y#(val$Y - 0.5) * 2
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)

### Validation
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

### Model Performance ###
v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v[,4]);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v[,5]);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

prediction <- as.factor(ifelse(v$INVEST_PERCENT >=0.5, 1, 0))
confusionMatrix(as.factor(v$PRED_PROFIT_LOSS_3), prediction)
