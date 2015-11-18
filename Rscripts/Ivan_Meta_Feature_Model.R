setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
load('data/Ivan_Train_Test_Scale_Center_20151116.RData');ls()
options(scipen=999);set.seed(19890624)

test <- train[train$EVENT_ID %in% c(101183757,101183885,101184013),]#validation
train <- train[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:(ncol(train)-3))] # train
feat <- feat[!feat %in% c('tsne_3d_1','tsne_3d_2','tsne_3d_3')]

########################################
### Meta model random forest ###########
########################################
OOBModel = randomForest(x=train[,feat], y=as.factor(train$flag_class), replace=F, ntree=200, do.trace=T, mtry=7)
bagPredictions_rf = predict(OOBModel, train, type="prob")
bagTestPredictions_rf = predict(OOBModel, test, type="prob")
bagValidPredictions_rf = predict(OOBModel, validation, type="prob")


#########################
### Xgboost #############
#########################
dtrain <- xgb.DMatrix(as.matrix(cbind(train[,feat],rf_n=bagPredictions_rf[,1],rf_y=bagPredictions_rf[,2])), label = train$flag_class)
dtest <- xgb.DMatrix(as.matrix(cbind(test[,feat],rf_n=bagTestPredictions_rf[,1],rf_y=bagTestPredictions_rf[,2])),label = test$flag_class)
dvalid <- xgb.DMatrix(as.matrix(cbind(validation[,feat],rf_n=bagValidPredictions_rf[,1],rf_y=bagValidPredictions_rf[,2])),label = validation$flag_class)
watchlist <- list(eval = dtest, train = dtrain)
bst <-
    xgb.train(
        data = dtrain, max.depth = 6, eta = 0.02, nround = 1200, maximize = F, min_child_weight = 2, colsample_bytree = 0.8,
        nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1, gamma = 0.1
        ,watchlist = watchlist
    )
p = predict(bst,dtest)
p = predict(bst,dvalid)


#########################
### Validation ##########
#########################
# val <- test
val <- validation
val$Y <- bagValidPredictions_rf[,2]
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
