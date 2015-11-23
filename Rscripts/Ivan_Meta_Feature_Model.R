setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
# load('data/9_train_validation_test_20151122.RData');ls()
load('data/v3/Ivan_train_test_20151115.RData');
options(scipen=999);set.seed(19890624)

test <- test#total[train$EVENT_ID %in% c(101183757,101183885,101184013),]
train <- total# total[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:(ncol(train)-3))] # train

########################################
### Meta model random forest ###########
########################################
# OOBModel = randomForest(x=train[,feat], y=as.factor(train$flag_class), replace=F, ntree=100, do.trace=T, mtry=7)
# bagPredictions_rf = predict(OOBModel, train, type="prob")
# bagTestPredictions_rf = predict(OOBModel, test, type="prob")
# bagValidPredictions_rf = predict(OOBModel, validation, type="prob")


#########################
### Xgboost #############
#########################
# train$flag_margin <- train$flag_regr/train$INVEST
# test$flag_margin <- test$flag_regr/test$INVEST
# validation$flag_margin <- validation$flag_regr/validation$INVEST
# reg:linear
# binary:logistic
dtrain <- xgb.DMatrix(as.matrix(train[,feat]), label = train$flag_class)
dtest <- xgb.DMatrix(as.matrix(test[,feat]),label = test$flag_class)
dvalid <- xgb.DMatrix(as.matrix(validation[,feat]),label = validation$flag_class)
watchlist <- list(eval = dvalid, train = dtrain)
# 1. GBM
for (i in 1:30){
    set.seed(19890624*i)
    bst <-
        xgb.train(
            data = dtrain, max.depth = 6, eta = 0.15, nround = 500, maximize = F, min_child_weight = 2, colsample_bytree = 0.7,
            nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', #num_parallel_tree = 1, gamma = 0.1,
            watchlist = watchlist
        )
    p_gbm = predict(bst,dtest)
    # p_gbm = predict(bst,dvalid)
    write.csv(p_gbm, paste0('ReadyForBlending/submission/xgboost/submission_xgboost_20151122_', i,'.csv'))
    # write.csv(p_gbm, paste0('submission_xgboost_20151122.csv'))
}

# # 2. RF
# bst <-
#     xgb.train(
#         data = dtrain, max.depth = 9, eta = 0.15, nround = 1, maximize = F, watchlist = watchlist, min_child_weight = 2, colsample_bytree = 0.8,
#         nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1000, gamma = 0.1
#     )
# # p_rf = predict(bst,dtest)
# p_rf = predict(bst,dvalid)
# 
# # 3. generalized linear model
#     bst <- xgb.train(
#         data = dtrain, nround = 100, watchlist = watchlist, objective = "binary:logistic", booster = "gblinear", eta = 0.3,
#         nthread = 4, alpha = 1e-3, lambda = 1e-6, print.every.n = 10
#     )
# # p_glm = predict(bst,dtest)
# p_glm = predict(bst,dvalid)

p <- 0.75*p_gbm + 0.25*p_glm

#########################
### Validation ##########
#########################
# val <- test
val <- validation
# p <- ifelse(p_gbm>0.5, 1, 0)
val$Y <- p_gbm
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

### New Calc
rocobj <- roc(val$flag_class, val$Y);print(auc(rocobj)) 
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
