setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
# devtools::install_github('dmlc/xgboost',subdir='R-package')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
# load('data/6_train_validation_test_center_scale_no_dummy.RData');ls()
load('data/6_train_validation_test_center_scale_no_dummy_2.RData');ls()

# Validation
set.seed(8)
inTraining <- createDataPartition(total$flag_class, p = .8, list = FALSE)
train <- total[inTraining,]
validation  <- total[-inTraining,]
dim(train); dim(validation)
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)


for(d in c(0.14, 0.16, 0.17, 0.18, 0.19, 0.2)){
    print (paste0('Parameter: ', d))
    
    #-------------Basic Training using XGBoost-----------------
    bst <- xgboost(data = as.matrix(train[,3:46]), label = train$flag_class, max.depth = 6, eta = 0.15, nround = 500,
                   nthread = 4, objective = "binary:logistic", verbose = 1)
    
    #----------------Advanced features --------------
    # dtrain <- xgb.DMatrix(data = as.matrix(train[,2:46]), label=as.matrix(train$flag_class))
    # dtest <- xgb.DMatrix(data = validation[,2:46], label=test$flag_class)
    # 
    # watchlist <- list(train=dtrain, test=dtest)
    # bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nround=2, watchlist=watchlist,
    #                  eval.metric = "error", eval.metric = "logloss",
    #                  nthread = 2, objective = "binary:logistic")
    
    
    #--------------------basic prediction using xgboost--------------
    val <- validation#[!validation$COUNTRY_OF_RESIDENCE_NAME %in% c('Qatar'),]
    p <- predict(bst, as.matrix(validation[,3:46]))
    val$Y <- p
    val$PRED_PROFIT_LOSS <- (val$Y - 0.5) * val$INVEST * 2
    pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=val, sum, na.rm=F)
    pred_fin$PRED_PROFIT_LOSS_2 <- ifelse(pred_fin$PRED_PROFIT_LOSS > 0, 1, ifelse(pred_fin$PRED_PROFIT_LOSS < 0, 0, 0.5))
    pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    
    ### Validation
    val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
    val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))
    
    #########################
    ### Model Performance ###
    #########################
    v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$PRED_PROFIT_LOSS_2);print(auc(rocobj)) # Invest * Possibility
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);print(auc(rocobj)) # Average Possibility
    # rocobj <- roc(val$flag_class, p);auc(rocobj) # Single Events 
    
    # Partial AUC:
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    
    
}


head(v[,c(3,6)], 100)


### Test 
train <- total

############
### test ###
############
p <- predict(bst, as.matrix(test[,3:46]))
test$Y <- p
pred_fin <- aggregate(Y ~ ACCOUNT_ID, data=test, mean, na.rm=F)

### Submission
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submission_20151031_gbm_c_s_nd.csv',quote = FALSE,row.names = FALSE)

