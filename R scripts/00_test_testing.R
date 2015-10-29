setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/train_validation_NEW.RData');

submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
head(submit)
head(test)

### Add Features to Test
names(test) <- c('ACCOUNT_ID', names(test)[-1])
test$TOT_BET_SIZE <- test$TRANSACTION_COUNT * test$AVG_BET_SIZE
test$STDEV_BET_SIZE_N <- test$AVG_BET_SIZE + test$STDEV_BET_SIZE
head(test)




test$row_num <- 1
test[duplicated(test[,c(1,2,4,5)]), 'row_num'] <- 2
View(test)

test$tot <- test$AVG_BET_SIZE * test$TRANSACTION_COUNT

test_s <- test[test$STATUS_ID == 'S',]
test_c <- test[test$STATUS_ID == 'C',]
test_l <- test[test$STATUS_ID == 'L',]

sum(test_s[test_s$row_num == 1, 'tot'])
sum(test_s[test_s$row_num == 2, 'tot'])

sum(test_c[test_c$row_num == 1, 'tot'])
sum(test_c[test_c$row_num == 2, 'tot'])

sum(test_l[test_l$row_num == 1, 'tot'])
sum(test_l[test_l$row_num == 2, 'tot'])

all$BLRatio <- (all$TRANSACTION_COUNT_INPLAY_b + all$TRANSACTION_COUNT_OUTPLAY_b)/(all$TRANSACTION_COUNT_INPLAY_l + all$TRANSACTION_COUNT_OUTPLAY_l + all$TRANSACTION_COUNT_INPLAY_b + all$TRANSACTION_COUNT_OUTPLAY_b)
range(all$BLRatio, na.rm=T)
