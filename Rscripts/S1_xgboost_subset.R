setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list = ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/S9_train_validation_test_20151110.RData');ls()

events <- unique(total$EVENT_ID)
for (i in 1:(length(events) - 2)) {
    print(paste0('Round ', i, ' | Subsetting without event: ', events[i:(i + 2)]))
    train <- total
    training <- train#[!train$EVENT_ID %in% events[i:(i + 2)],]
    training$flag_class <- ifelse(training$flag_class == 'Y', 1, 0)
    feat <- colnames(training)[c(3:56,59:61,64:66)]
    
    #-------------Basic Training using XGBoost-----------------
    bst <-
        xgboost(
            data = as.matrix(training[,feat]), label = training$flag_class, max.depth = 6, eta = 0.02, nround = 1200, maximize = F, #500,0.15
            nthread = 4, objective = "binary:logistic", metrics = 'auc', verbose = 0
        )
    
#     bst <-
#         xgboost(
#             data = as.matrix(training[,feat]), label = training$flag_class, max.depth = 9, num_parallel_tree = 150, subsample = 0.5, colsample_bytree =
#                 0.5, nround = 1, objective = "binary:logistic"
#         )
    
    #-------------Test prediction-----------------
    p <- predict(bst, as.matrix(test[,feat]))
    
    if (i == 1) {
        pred <- p
    }else{
        pred <- pred + p
    }
}

submit <- pred / (length(events) - 2)

write.csv(
    as.data.frame(submit),'pred/submission_20151106_xg_gbm.csv',quote = FALSE,row.names = FALSE
)
write.csv(
    as.data.frame(submit),'pred/submission_20151106_xg_rf.csv',quote = FALSE,row.names = FALSE
)





p <- predict(bst, as.matrix(test[,feat]))
t <- test
t$Y <- p
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=t, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
t <- merge(t, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
t$INVEST_PERCENT <- t$INVEST/t$TOT_INVEST * t$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=t, sum, na.rm=F)

### Submission
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submission_20151108_metafeat_v1.csv',quote = FALSE,row.names = FALSE)
