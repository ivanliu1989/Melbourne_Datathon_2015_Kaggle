setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret)
load('../S9_train_validation_test_20151110.RData');ls()
options(scipen=999);set.seed(1004)

train <- train
test <- validation
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:(ncol(train)-2))]
# x = 1/(1+exp(-sqrt(x)))

#############################
### Raw prediction ##########
#############################
dtrain <- xgb.DMatrix(as.matrix(train[,feat]), label = train$flag_class)
dtest <- xgb.DMatrix(as.matrix(test[,feat]), label = test$flag_class)
watchlist <- list(eval = dtest, train = dtrain)

# # Train the model
    bst <-
        xgb.train(
            data = dtrain, max.depth = 6, eta = 0.15, nround = 500, maximize = F, watchlist = watchlist, min_child_weight = 4, colsample_bytree = 0.8,
            nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1, gamma = 0.1
        )

# # Make prediction
testPredictions = predict(bst,dtest)

#################################
### Meta bagging Model ##########
#################################
# testPredictions <- matrix(0, nrow = nrow(test), ncol = 1)
bootRounds = 1:10

for (j in bootRounds) {
  print(j)
  # Do the bootstrap resampling
  baggedIndex = sample(nrow(train), size = nrow(train), replace = T)
  OOBIndex = setdiff(1:nrow(train), baggedIndex)
  
  # Build the OOB model
  OOBModel = randomForest(x=train[OOBIndex,feat], y=as.factor(train$flag_class[OOBIndex]), replace=F, ntree=100, do.trace=T, mtry=7)
  bagPredictions = predict(OOBModel, train[baggedIndex,], type="prob")
  bagTestPredictions = predict(OOBModel, test, type="prob")
  
  # Build the Bag model
  dtrain <- xgb.DMatrix(as.matrix(train[baggedIndex,feat]), label = train$flag_class[baggedIndex])
  dtest <- xgb.DMatrix(as.matrix(cbind(test[,feat],bagTestPredictions)), label = test$flag_class)
  watchlist <- list(eval = dtest, train = dtrain)
  
  BagModel <- xgb.train(data = dtrain, max.depth = 6, eta = 0.15, nround = 500, watchlist = watchlist, #min_child_weight = 4, colsample_bytree = 0.8,
                        nthread = 4, objective = "binary:logistic"#, metrics = 'auc', gamma = 0.1
  )
  
  # Make test predictions and reshape them
  tmpPredictions = predict(BagModel,dtest)
  testPredictions = testPredictions + tmpPredictions
}
testPredictions = testPredictions/(j+1)

#########################
### Validation ##########
#########################
val <- test
val$Y <- testPredictions
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, sum, na.rm=F)
pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

### Model Performance ###
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin[,2]);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin2[,2]);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

prediction <- as.factor(ifelse(pred_fin[,2] >=0.5, 1, 0))
confusionMatrix(as.factor(val_fin$PRED_PROFIT_LOSS_3), prediction)
