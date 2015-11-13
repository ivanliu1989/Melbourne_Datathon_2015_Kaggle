setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia)
# load('../S9_train_validation_test_20151110.RData');ls()
load('data/S9_train_validation_test_20151110_test.RData');ls()
options(scipen=999);set.seed(19890624)

train <- total
test <- test
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- 0
# feat <- colnames(train)[c(3:(ncol(train)-2))] # train
feat <- colnames(train)[c(3:(ncol(train)-3), ncol(train))] # test

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
testPredictions_xb = predict(bst,dtest)
# testPredictions_nn = predict(bst,dtest)

#################################
### Meta bagging Model ##########
#################################
# testPredictions <- matrix(0, nrow = nrow(test), ncol = 1)
bootRounds = 1:50

for (j in bootRounds) {
    print(j)
    # Do the bootstrap resampling
    baggedIndex = sample(nrow(train), size = nrow(train), replace = T)
    OOBIndex = setdiff(1:nrow(train), baggedIndex)
    
    # Build the OOB model
    # randomforest
    OOBModel = randomForest(x=train[OOBIndex,feat], y=as.factor(train$flag_class[OOBIndex]), replace=F, ntree=100, do.trace=T, mtry=7)
    bagPredictions_rf = predict(OOBModel, train[baggedIndex,], type="prob")
    bagTestPredictions_rf = predict(OOBModel, test, type="prob")
    
    # sofia svm
    train_svm <- train[baggedIndex, feat];
    train_svm_t <- train[OOBIndex, feat]; test_svm <- test[,feat]
    prep <- preProcess(train_svm, method = c('center',"scale"), verbose =T)
    train_svm <- cbind(predict(prep, train_svm),flag_class=train$flag_class[baggedIndex])
    train_svm_t <- cbind(predict(prep, train_svm_t),flag_class=train$flag_class[OOBIndex])
    test_svm <- cbind(predict(prep, test_svm),flag_class=test$flag_class)
    fit <- sofia(flag_class ~ ., data=train_svm_t, lambda = 1e-3, iiterations = 1e+25, random_seed = 13560,
                 learner_type = 'logreg-pegasos', eta_type = 'pegasos', loop_type = 'balanced-stochastic', 
                 rank_step_probability = 0.5, passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 78,
                 perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0
    )
    bagPredictions_svm <- predict(fit, newdata=train_svm, prediction_type = "logistic")
    bagTestPredictions_svm <- predict(fit, newdata=test_svm, prediction_type = "logistic")
    
    # xg glm
    dtrain <- xgb.DMatrix(as.matrix(train[OOBIndex,feat]), label = train$flag_class[OOBIndex])
    dtrain_t <- xgb.DMatrix(as.matrix(train[baggedIndex,feat]), label = train$flag_class[baggedIndex])
    dtest <- xgb.DMatrix(as.matrix(test[,feat]), label = test$flag_class)
    bst <- xgb.train(
        data = dtrain, nround = 500, objective = "binary:logistic", booster = "gblinear", eta = 0.1,
        nthread = 4, alpha = 1e-3, lambda = 1e-6, verbose = 0
    )
    bagPredictions_glm = predict(bst, dtrain_t)
    bagTestPredictions_glm = predict(bst, dtest)
    
    # Build the Bag model
    dtrain <- xgb.DMatrix(as.matrix(cbind(train[baggedIndex,feat],bagPredictions_rf,svm=bagPredictions_svm,glm=bagPredictions_glm)), 
                          label = train$flag_class[baggedIndex])
    dtest <- xgb.DMatrix(as.matrix(cbind(test[,feat],bagTestPredictions_rf,svm=bagTestPredictions_svm,glm=bagTestPredictions_glm)), 
                         label = test$flag_class)
    # watchlist <- list(eval = dtest, train = dtrain)
    
    BagModel <- xgb.train(data = dtrain, max.depth = 6, eta = 0.15, nround = 500, #watchlist = watchlist, 
                          colsample_bytree = 0.8, min_child_weight = 10, verbose = 0, 
                          nthread = 4, objective = "binary:logistic"#, metrics = 'auc', gamma = 0.1
    )
    
    # Make test predictions and reshape them
    tmpPredictions = predict(BagModel,dtest)
    testPredictions_xb = testPredictions_xb + tmpPredictions
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
