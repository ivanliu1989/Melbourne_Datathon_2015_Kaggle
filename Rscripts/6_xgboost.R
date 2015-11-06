setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_20151105.RData');ls()

### Test
train <- total
### Validation
set.seed(18)
dim(train); dim(validation)
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:56,58:59)]

# feat <- feat[
#     !feat %in%
#         c('MAX_BET_SIZE_OUTPLAY_L',
#           'AVG_PLACED_TAKEN_TIME_INPLAY',
#           'STDEV_BET_SIZE_OUTPLAY',
#           'AVG_BET_SIZE_OUTPLAY',
#           'BL_DIFF_STDEV_BET_SIZE_OUT',
#           'KURT_PLACED_TAKEN_TIME_INPLAY',
#           'NET_PROFIT_INPLAY',
#           'STDEV_BET_SIZE_INPLAY',
#           'TRANSACTION_COUNT_OUTPLAY_L',
#           'SKEW_PLACED_TAKEN_TIME_INPLAY',
#           'TRANSACTION_COUNT_INPLAY',
#           'BL_DIFF_TRANSACTION_COUNT_IN',
#           'INPLAY_RATIO',
#           'win_hist')]

feat <- feat[
    !feat %in%
        c('kmeans')]
# for(d in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12,0.13,0.14,0.15)){
    # print (paste0('Parameter: ', d))
    
    #-------------Basic Training using XGBoost-----------------
    bst <-
        xgboost(  # c(3:22,42,43,47:56) | 3:56
            data = as.matrix(train[,feat]), label = train$flag_class, max.depth = 6, eta = 0.15, nround = 500, maximize = F,
            nthread = 4, objective = "binary:logistic", verbose = 1, early.stop.round = 10, print.every.n = 10, metrics = 'auc'
        )
    
    #-------------Tuning using XGBoost-----------------
    #     bst <- xgb.cv(data = as.matrix(train[,3:46]), label = train$flag_class, nround = 5000, max.depth = 6, eta = 0.02, nfold = 5,
    #                   prediction = F, showsd = T, stratified = T, objective = "binary:logistic", #metrics = 'auc',# 'rmse', 'logloss', 'error', 'auc'
    #                   verbose = 1, early.stop.round = 50, print.every.n = 5, maximize = F)
    
    #--------------------basic prediction using xgboost--------------
    val <- validation
    # for (col in names(val[,-c(1:22,42,43,47:59)])){
    #     val[, col] <- median(all[,col], na.rm = T)
    # }  
    p <- predict(bst, as.matrix(val[,feat])) 
    # p <- predict(bst, as.matrix(train[,feat])) 
    val$Y <- p
    
    tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
    val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * (val$Y - 0.5) * 2
    pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    
    ### Validation
    val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
    val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))
    
    #########################
    ### Model Performance ###
    #########################
    v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);print(auc(rocobj)) # Invest * Possibility
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);print(auc(rocobj)) # Average Possibility
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    
    # roc(val$flag_class, p)
    write.csv(as.data.frame(p),file=paste0('ReadyForBlending/validation/1_xg_0.9635_0.9334.csv'),quote = FALSE,row.names = FALSE)
    write.csv(as.data.frame(p),file=paste0('ReadyForBlending/validation/2_xg_train.csv'),quote = FALSE,row.names = FALSE)
# }
    
############
### test ###
############
p <- predict(bst, as.matrix(test[,feat]))
# test$Y <- p
# pred_fin <- aggregate(Y ~ ACCOUNT_ID, data=test, mean, na.rm=F)
# 
# ### Submission
# submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
# names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
# submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
# table(is.na(submit$PRED_PROFIT_LOSS))
# submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
# submit$Prediction <- submit$PRED_PROFIT_LOSS
# submit$PRED_PROFIT_LOSS <- NULL

write.csv(as.data.frame(p),'pred/submission_20151105_xg_blend.csv',quote = FALSE,row.names = FALSE)

