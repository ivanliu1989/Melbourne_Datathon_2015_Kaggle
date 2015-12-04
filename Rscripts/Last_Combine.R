setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
load('data/9_train_validation_test_20151122.RData');ls()
test_22 <- test;
total_22 <- total;
test_n_22 <- test_n;

load('data/v5/Ivan_Train_Test_Scale_Center_20151123.RData');ls()
test_23 <- test;
total_23 <- total;
test_n_23 <- test_n;

dim(total_23); dim(total_22)
total_22$identifier <- paste0(total_22$ACCOUNT_ID, total_22$EVENT_ID)
total_23$identifier <- paste0(total_23$ACCOUNT_ID, total_23$EVENT_ID)
total_22 <- total_22[total_22$identifier %in% total_23$identifier, ]
total_22$identifier <- NULL

total <- merge(total_22[,1:59],total_23[,c('ACCOUNT_ID','EVENT_ID', 'PAST_LAG_1')]
               ,all.x = T, all.y = T, by = c('ACCOUNT_ID','EVENT_ID'))
dim(total)
total <- total[,c(1:56,60,57:59)]

test <- merge(test_22[,1:59],test_23[,c('ACCOUNT_ID','EVENT_ID', 'PAST_LAG_1')]
               ,all.x = F, all.y = T, by = c('ACCOUNT_ID','EVENT_ID'))
test <- test[,c(1:56,60,57:59)]

total_final <- total
test_final <- test

total <- total_final
test <- test_final
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
save(total,test,test_n,train,validation, file='data/9_train_validation_test_20151202.RData')
