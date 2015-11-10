setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/S9_train_validation_test_20151110.RData');ls()
# c(101183757,101183885,101184013) - last 3 event
# c(101150834,101153072,101149398) - validation
# c(101093076,101093194,101093312) 
# c(101128387,101150348,101152275) 
# c(101149870,101150716,101153308)

### Test
# train <- total
### Validation
training <- train[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
testing <- train[train$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(training); dim(testing)
training$flag_class <- ifelse(training$flag_class == 'Y', 1, 0)
feat <- colnames(training)[c(3:70)]#c(3:72) #3:13,16,17,20,21,24,25,28,29,32,44:72

# feat <- feat[
#     !feat %in%
#         c('STDEV_BET_SIZE_OUTPLAY_L','BL_DIFF_TRANSACTION_COUNT_OUT','TRANSACTION_COUNT_OUTPLAY_L',
#           'BL_DIFF_STDEV_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','AVG_TAKEN_HOUR_INPLAY')]

# for(d in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12,0.13,0.14,0.15)){
    # print (paste0('Parameter: ', d))
    
    #-------------Basic Training using XGBoost-----------------
    bst <-
        xgboost( 
            data = as.matrix(training[,feat]), label = training$flag_class, max.depth = 6, eta = 0.15, nround = 500, maximize = F, #500,0.15
            nthread = 4, objective = "binary:logistic", verbose = 1, early.stop.round = 10, print.every.n = 10, metrics = 'auc'
        )
#     bst <-
#         xgboost(
#             data = as.matrix(training[,feat]), label = training$flag_class, max.depth = 9, num_parallel_tree = 150, subsample = 0.5, colsample_bytree =
#                 0.5, nround = 1, objective = "binary:logistic"
#         )

    #--------------------basic prediction using xgboost--------------
    val <- validation
    # val <- testing
    p <- predict(bst, as.matrix(val[,feat])) 
    # p <- predict(bst, as.matrix(train[,feat])) 
    val$Y <- p
    
    tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
    val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y#(val$Y - 0.5) * 2
    pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    
    ### Validation
    val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
    val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)
    
    #########################
    ### Model Performance ###
    #########################
    v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);print(auc(rocobj)) # Invest * Possibility
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);print(auc(rocobj)) # Average Possibility
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    
    prediction <- as.factor(ifelse(v$INVEST_PERCENT >=0.5, 1, 0))
    confusionMatrix(as.factor(v$PRED_PROFIT_LOSS_3), prediction)
    
    #################################
    ### Plot & feature importance ###
    #################################
    model <- xgb.dump(bst, with.stats = T)
    model[1:10]
    # Get the feature real names
    names <- dimnames(as.matrix(train[,feat]))[[2]]
    # Compute feature importance matrix
    importance_matrix <- xgb.importance(names, model = bst)
    # Nice graph
    xgb.plot.importance(importance_matrix)
    xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 1)
    
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

