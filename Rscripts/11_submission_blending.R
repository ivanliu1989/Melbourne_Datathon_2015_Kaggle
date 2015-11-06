setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
load('data/9_train_validation_test_20151105.RData');ls()
path <- 'ReadyForBlending/submission/final/'
file.names <- list.files(path)


# for(file in 1:length(file.names)){
#     p <- read.csv(paste0(path, file.names[file]))
#     if(file == 1){pred <- p}
#     else{
#         pred[,2]<-pred[,2]+p[,2]
#         if(file==length(file.names)) pred[,2] <- pred[,2]/file
#         }
# }
# head(pred)

p1 <- read.csv(paste0(path, file.names[1]))[,3]
p2 <- read.csv(paste0(path, file.names[2]))[,3]
p3 <- read.csv(paste0(path, file.names[3]))[,3]
p4 <- read.csv(paste0(path, file.names[4]))[,3]
p5 <- read.csv(paste0(path, file.names[5]))
Y <- (p1+p2+p3+p4+p5)/5; names(Y) <- 'Y'
# Y <- p5
t <- test
t$Y <- Y[,1]
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=t, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
t <- merge(t, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
t$INVEST_PERCENT <- t$INVEST/t$TOT_INVEST * (t$Y - 0.5) * 2
# pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=t, mean, na.rm=F)
pred_fin <- aggregate(Y ~ ACCOUNT_ID, data=t, mean, na.rm=F)

### Submission
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

# write.csv(submit,'ReadyForBlending/submission/submission_20151103_h2o_dl_blend.csv',quote = FALSE,row.names = FALSE)
write.csv(submit,'pred/submission_20151105_blend_v2.csv',quote = FALSE,row.names = FALSE)
