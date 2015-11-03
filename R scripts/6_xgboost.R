setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
# rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_TREE_1.RData');ls()
# load('data/9_train_validation_test_ONEHOT_1.RData');ls()

### Test
train <- total
### Validation
set.seed(808)
dim(train); dim(validation)
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
feat <- c(3:56,59)

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
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    
    # roc(val$flag_class, p)
    write.csv(as.data.frame(p),file=paste0('ReadyForBlending/validation/1_xg_0.9361_0.88886.csv'),quote = FALSE,row.names = FALSE)
    write.csv(as.data.frame(p),file=paste0('ReadyForBlending/validation/2_xg_train.csv'),quote = FALSE,row.names = FALSE)
# }
    
############
### test ###
############
p <- predict(bst, as.matrix(test[,feat]))
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

write.csv(submit,'pred/submission_20151103_gbm_blend.csv',quote = FALSE,row.names = FALSE)

