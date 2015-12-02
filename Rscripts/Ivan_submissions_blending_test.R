setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
load('data/9_train_validation_test_20151122.RData');ls()

submit_xgb_gbm <- list.files('ReadyForBlending/submission/test_n/xgboost_gbm/', full.names = T)
submit_vw <- list.files('ReadyForBlending/submission/test_n/vw/', full.names = T)
submit_lasagne <- list.files('ReadyForBlending/submission/test_n/lasagne/', full.names = T)
submit_fm <- list.files('ReadyForBlending/submission/test_n/libffm/', full.names = T)
# submit_svm <- list.files('ReadyForBlending/submission/test_n/libsvm/', full.names = T)
submit_h2o_lg <- list.files('ReadyForBlending/submission/test_n/h2o_glm/', full.names = T)
submit_h2o_rf <- list.files('ReadyForBlending/submission/test_n/h2o_rf/', full.names = T)
submit_h2o_nnet <- list.files('ReadyForBlending/submission/test_n/h2o_nnet/', full.names = T)
submit_xgb_glm <- list.files('ReadyForBlending/submission/test_n/xgboost_glm/', full.names = T)


combine_results <- function(files, header, sep=','){
    for(f in 1:length(files)){
        #print(f)
        if(f == 1){
            pred <- read.csv(files[f], header = header, sep = sep)
        }else{
            pred <- pred + read.csv(files[f], header = header, sep=sep)
        }
        if(f == length(files)){
            pred <- pred/f
        }
    }
    return(pred)
}

pred_xgb_gbm <- combine_results(submit_xgb_gbm, header = TRUE); head(pred_xgb_gbm); dim(pred_xgb_gbm)
pred_xgb_glm <- combine_results(submit_xgb_glm, header = TRUE); head(pred_xgb_glm); dim(pred_xgb_glm)
pred_vw <- combine_results(submit_vw, header = FALSE); head(pred_vw); dim(pred_vw)
pred_lasagne <- combine_results(submit_lasagne, header = FALSE); head(pred_lasagne); dim(pred_lasagne)
pred_fm <- combine_results(submit_fm, header = FALSE); head(pred_fm); dim(pred_fm)
# pred_svm <- combine_results(submit_svm, header = TRUE, sep = ' '); head(pred_svm); dim(pred_svm)
pred_h2o_lg <- combine_results(submit_h2o_lg, header = TRUE); head(pred_h2o_lg); dim(pred_h2o_lg)
pred_h2o_rf <- combine_results(submit_h2o_rf, header = TRUE); head(pred_h2o_rf); dim(pred_h2o_rf)
pred_h2o_nnet <- combine_results(submit_h2o_nnet, header = TRUE); head(pred_h2o_nnet); dim(pred_h2o_nnet)


pred_all_n <- (0.4*pred_xgb_gbm[,2] +  #30
                 
                 0.1*pred_xgb_glm[,2] +  #20
                 0.05*pred_h2o_lg[,4] + 
                 0.05*pred_vw[,1] + 
                 
                 0.25*pred_lasagne[,2] +  #25
                 # 0.1*pred_h2o_nnet[,4] +
                 
                 0.1*pred_fm[,1] + #10
                 # 0.1*pred_svm[,3] + #10
                 
                 0.05*pred_h2o_rf[,4] #5
)
head(pred_all_n); length(pred_all_n)

#########################
### Submission ##########
#########################
t <- test_n
t$Y <- pred_all_n
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=t, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
t <- merge(t, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
t$INVEST_PERCENT <- t$INVEST/t$TOT_INVEST * t$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=t, sum, na.rm=F)
# pred_fin <- aggregate(Y ~ ACCOUNT_ID, data=t, mean, na.rm=F)
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')

submit_n <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
submit_n <- merge(submit_n,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit_n$PRED_PROFIT_LOSS))
submit_n <- submit_n[!is.na(submit_n$PRED_PROFIT_LOSS),]
submit_n$Prediction <- submit_n$PRED_PROFIT_LOSS
submit_n$PRED_PROFIT_LOSS <- NULL

# write.csv(submit,'pred/submission_20151202_9_blend_30_20_25_10_10.csv',quote = FALSE,row.names = FALSE)

