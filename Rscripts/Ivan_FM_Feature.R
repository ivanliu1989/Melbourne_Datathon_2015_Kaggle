setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
# load('data/Ivan_Train_Test_Scale_Center_20151116.RData');ls()
load('data/9_train_validation_test_20151122.RData');ls()
options(scipen=999);set.seed(19890624)
# 
# write.csv(test, '../python_test_ffm_meta.csv', row.names = F)
# write.csv(train, '../python_train_ffm_meta.csv', row.names = F)
# write.csv(validation, '../python_validation_ffm_meta.csv', row.names = F)
# write.csv(total, '../python_total_ffm_meta.csv', row.names = F)
# write.csv(p_gbm, 'ReadyForBlending/xgboost_1.csv', row.names = F)

#########################
### Validation ##########
#########################
p <- read.csv('PythonScripts/lasagne/V1/lasagne_3L_tsne2_ffm_1.csv', header = F);val <- validation; val$Y <- p[,2]
p <- read.csv('libffm/output_file.csv', header = F);val <- validation; val$Y <- p[,1]
p <- read.table('vowpal_wabbit/predictions/out.txt', header = F);val <- validation; val$Y <- p[,1]
p <- read.csv('ReadyForBlending/xgboost_1.csv', header = T);val <- validation; val$Y <- p[,1]
p <- read.csv('libffm/output_file.csv', header = T, sep = " ");val <- validation; val$Y <- p[,3]


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

p_vw <- p[,1]
p_ffm <- p[,1]
p_nnet <- p[,2]
p_gbm <- p[,1]
p <- 0.05*p_vw + 0.25*p_nnet + 0.7*p_gbm # + 0.05*p_ffm
val <- validation; val$Y <- p

#########################
### Submission ##########
#########################
t <- test
t$Y <- p[,2]
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=t, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
t <- merge(t, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
t$INVEST_PERCENT <- t$INVEST/t$TOT_INVEST * t$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=t, sum, na.rm=F)
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')

submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'PythonScripts/lasagne/nnet_3layers2_2_agg.csv',quote = FALSE,row.names = FALSE)
