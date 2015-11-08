setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
test <- test[test$STATUS_ID == 'S',]
head(test)

library(data.table)
test <- data.table(test)

### campute lag
test[, lag_TRANSACTION_COUNT:=c(0, TRANSACTION_COUNT[-.N]), by=c('Account_ID','EVENT_ID','INPLAY_BET')]
test[, lag_AVG_BET_SIZE:=c(0, AVG_BET_SIZE[-.N]), by=c('Account_ID','EVENT_ID','INPLAY_BET')]
test[, lag_MAX_BET_SIZE:=c(0, MAX_BET_SIZE[-.N]), by=c('Account_ID','EVENT_ID','INPLAY_BET')]
test[, lag_MIN_BET_SIZE:=c(0, MIN_BET_SIZE[-.N]), by=c('Account_ID','EVENT_ID','INPLAY_BET')]
test[, lag_STDEV_BET_SIZE:=c(0, STDEV_BET_SIZE[-.N]), by=c('Account_ID','EVENT_ID','INPLAY_BET')]

### aggregate
test <- as.data.frame(test)
test$bl_diff_TRANSACTION_COUNT <- abs(test$TRANSACTION_COUNT - test$lag_TRANSACTION_COUNT)/(test$TRANSACTION_COUNT + test$lag_TRANSACTION_COUNT)
test$bl_diff_AVG_BET_SIZE<- abs(test$AVG_BET_SIZE - test$lag_AVG_BET_SIZE)/(test$AVG_BET_SIZE + test$lag_AVG_BET_SIZE)
test$bl_diff_MAX_BET_SIZE<- abs(test$MAX_BET_SIZE - test$lag_MAX_BET_SIZE)/(test$MAX_BET_SIZE + test$lag_MAX_BET_SIZE)
test$bl_diff_MIN_BET_SIZE<- abs(test$MIN_BET_SIZE - test$lag_MIN_BET_SIZE)/(test$MIN_BET_SIZE + test$lag_MIN_BET_SIZE)
test$bl_diff_STDEV_BET_SIZE<- abs(test$STDEV_BET_SIZE - test$lag_STDEV_BET_SIZE)/(test$STDEV_BET_SIZE + test$lag_STDEV_BET_SIZE)

test$row_num <- 1
test$identifier <- paste0(test$Account_ID, test$EVENT_ID, test$STATUS_ID, test$INPLAY_BET)
test[duplicated(test[,c(1,2,4,5)]), 'row_num'] <- 2

test_feat_bl <- test[test$row_num == 2, c(1,2,4,5, 15:19, 21)]
test_feat <- test[!test$identifier %in% test_feat_bl$identifier, c(1,2,4,5, 15:19, 21)]

test_feat_final <- rbind(test_feat_bl, test_feat)

### Output
dim(test_feat_final);length(unique(test_feat_final$identifier))
test_feat_final$identifier <- NULL


### 1-0
# 0 no difference
# 1 purchased either B or L
# NA -> 0

### test_feat merge
test_feat_inplay <- test_feat_final[test_feat_final$INPLAY_BET == 'Y',-c(3,4,10)]; 
names(test_feat_inplay) <- c('ACCOUNT_ID', 'EVENT_ID', 'BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                             'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN')
test_feat_outplay <- test_feat_final[!test_feat_final$INPLAY_BET == 'Y',-c(3,4,10)]
names(test_feat_outplay) <- c('ACCOUNT_ID', 'EVENT_ID', 'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                              'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')
load('data/2_test.RData')
head(test)
test <- merge(test, test_feat_inplay, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test <- merge(test, test_feat_outplay, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test[,c('BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
        'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
        'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
        'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')][is.na(test[,c('BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                                                                                                           'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
                                                                                                           'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                                                                                                           'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')])] <- 0

tail(test, 100)
