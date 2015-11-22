setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
load('data/9_train_validation_test_20151122.RData');ls()

submit_xgb <- list.files('ReadyForBlending/submission/xgboost/', full.names = T)
submit_vw <- list.files('ReadyForBlending/submission/vw/', full.names = T)
submit_lasagne_1 <- list.files('ReadyForBlending/submission/lasagne/', full.names = T)
submit_lasagne_2 <- list.files('ReadyForBlending/submission/lasagne_no_tsne/', full.names = T)

combine_results <- function(files, header){
    for(f in 1:length(files)){
        #print(f)
        if(f == 1){
            pred <- read.csv(files[f], header = header)
        }else{
            pred <- pred + read.csv(files[f], header = header)
        }
        if(f == length(files)){
            pred <- pred/f
        }
    }
    return(pred)
}

pred_xgb <- combine_results(submit_xgb, header = TRUE); head(pred_xgb); dim(pred_xgb)
pred_vw <- combine_results(submit_vw, header = FALSE); head(pred_vw); dim(pred_vw)
pred_lasagne_1 <- combine_results(submit_lasagne_1, header = FALSE); head(pred_lasagne_1); dim(pred_lasagne_1)
pred_lasagne_2 <- combine_results(submit_lasagne_2, header = FALSE); head(pred_lasagne_2); dim(pred_lasagne_2)
pred_lasagne <- (pred_lasagne_1 + pred_lasagne_2)/2; head(pred_lasagne); dim(pred_lasagne)

pred_all <- 0.1*pred_vw[,1] + 0.3*pred_lasagne[,2] + 0.6*pred_xgb[,2]; head(pred_all)

#########################
### Submission ##########
#########################
t <- test
t$Y <- p_gbm
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=t, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
t <- merge(t, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
t$INVEST_PERCENT <- t$INVEST/t$TOT_INVEST * t$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=t, sum, na.rm=F)
# pred_fin <- aggregate(Y ~ ACCOUNT_ID, data=t, mean, na.rm=F)
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')

submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0.43
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submission_20151122.csv',quote = FALSE,row.names = FALSE)






load('data/v3/Ivan_train_test_20151115.RData');ls()
train_new <- train
load('data/9_train_validation_test_20151122.RData')
View(train_new); View(train)
require(data.table)
