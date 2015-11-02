setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
# devtools::install_github('dmlc/xgboost',subdir='R-package')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_TREE_1.RData');ls()
# load('data/9_train_validation_test_ONEHOT_1.RData');ls()

# Validation
set.seed(8)
inTraining <- createDataPartition(total$flag_class, p = .8, list = FALSE)
train <- total[inTraining,]
validation  <- total[-inTraining,]
dim(train); dim(validation)
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)


for(d in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12,0.13,0.14,0.15)){
    print (paste0('Parameter: ', d))
    
    #-------------Basic Training using XGBoost-----------------
    bst <- xgboost(data = as.matrix(train[,3:56]), label = train$flag_class, max.depth = 6, eta = 0.13, nround = 1400, maximize = F, 
                   objective = "binary:logistic", verbose = 1, early.stop.round = 10, print.every.n = 50) #nthread = 4, 
    
    #-------------Tuning using XGBoost-----------------  
#     bst <- xgb.cv(data = as.matrix(train[,3:46]), label = train$flag_class, nround = 5000, max.depth = 6, eta = 0.02, nfold = 5, 
#                   prediction = F, showsd = T, stratified = T, objective = "binary:logistic", #metrics = 'auc',# 'rmse', 'logloss', 'error', 'auc'
#                   verbose = 1, early.stop.round = 50, print.every.n = 5, maximize = F)
    
    #--------------------basic prediction using xgboost--------------
    val <- validation#[!validation$COUNTRY_OF_RESIDENCE_NAME %in% c('Qatar'),]
    p <- predict(bst, as.matrix(validation[,3:56]))
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
    v <- merge(val_fin,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
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

