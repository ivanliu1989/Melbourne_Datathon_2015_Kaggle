setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret)
load('../S9_train_validation_test_20151110.RData');ls()
options(scipen=999);set.seed(19890624)

train <- total
test <- test
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:(ncol(train)-2))]
# x = 1/(1+exp(-sqrt(x)))

#############################
### Raw prediction ##########
#############################
dtrain <- xgb.DMatrix(as.matrix(train[,feat]), label = train$flag_class)
dtest <- xgb.DMatrix(as.matrix(test[,feat]), label = test$flag_class)

# # Train the model
bst <-
    xgb.train(
        data = dtrain, max.depth = 6, eta = 0.02, nround = 1200, maximize = F, min_child_weight = 3, colsample_bytree = 0.8,
        nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1, gamma = 0.1
    )

# # Make prediction
testPredictions = predict(bst,dtest)

#################################
### Meta bagging Model ##########
#################################
# testPredictions <- matrix(0, nrow = nrow(test), ncol = 1)
bootRounds = 1:200

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
    dtrain <- xgb.DMatrix(as.matrix(cbind(train[baggedIndex,feat],bagPredictions)), label = train$flag_class[baggedIndex])
    dtest <- xgb.DMatrix(as.matrix(cbind(test[,feat],bagTestPredictions)), label = test$flag_class)
    
    BagModel <- xgb.train(data = dtrain, max.depth = 6, eta = 0.46, nround = 60, #watchlist = watchlist, 
                          colsample_bytree = 0.8, min_child_weight = 10, verbose = 0, 
                          nthread = 4, objective = "binary:logistic"#, metrics = 'auc', gamma = 0.1
    )
    
    # Make test predictions and reshape them
    tmpPredictions = predict(BagModel,dtest)
    testPredictions = testPredictions + tmpPredictions
}
testPredictions = testPredictions/(j+1)

#########################
### Submission ##########
#########################
t <- test
t$Y <- testPredictions
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=t, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
t <- merge(t, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
t$INVEST_PERCENT <- t$INVEST/t$TOT_INVEST * t$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=t, sum, na.rm=F)

submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submit_20151112_meta_bagged_model_with_new_feat_part1.csv',quote = FALSE,row.names = FALSE)
