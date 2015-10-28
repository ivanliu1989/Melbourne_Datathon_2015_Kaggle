setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/train_validation.RData');ls()

submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
head(submit)
head(test)

### Add Features to Test
names(test) <- c('ACCOUNT_ID', names(test)[-1])
test_dt <- test[!duplicated(test[,c(1,2)]),c(1,2)]
  
### MERGE AND RETURN Member base
# Event specific
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c(1,2,3)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c(1,2,3)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c(1,2,3)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c(1,2,3)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c(1,2,3)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c(1,2,3)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c(1,2,6)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c(1,2,6)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c(1,2,6)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c(1,2,6)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c(1,2,6)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c(1,2,6)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c(1,2,7)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c(1,2,7)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c(1,2,7)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c(1,2,7)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c(1,2,7)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c(1,2,7)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c(1,2,8)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c(1,2,8)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c(1,2,8)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c(1,2,8)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c(1,2,8)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c(1,2,8)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c(1,2,9)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c(1,2,9)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c(1,2,9)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c(1,2,9)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c(1,2,9)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c(1,2,9)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

head(test_dt)

# Overall
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME[AVG_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME[AVG_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME[STDEV_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME[STDEV_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

test_dt <- merge(test_dt, AVG_TAKEN_HOUR[AVG_TAKEN_HOUR$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_TAKEN_HOUR[AVG_TAKEN_HOUR$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

test_dt <- merge(test_dt, PREV_WIN_RATE[PREV_WIN_RATE$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, PREV_WIN_RATE[PREV_WIN_RATE$INPLAY_BET == 'N', c(1,4,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

test_dt <- merge(test_dt, NET_PROFIT[NET_PROFIT$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, NET_PROFIT[NET_PROFIT$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

test_dt <- merge(test_dt, MARGIN[MARGIN$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MARGIN[MARGIN$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

names(test_dt) <- c('ACCOUNT_ID','EVENT_ID','TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_INPLAY_L','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_INPLAY_C','TRANSACTION_COUNT_OUTPLAY_C',
                      'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_INPLAY_L','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_INPLAY_C','AVG_BET_SIZE_OUTPLAY_C',
                      'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_INPLAY_L','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_INPLAY_C','MAX_BET_SIZE_OUTPLAY_C',
                      'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_INPLAY_L','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_INPLAY_C','MIN_BET_SIZE_OUTPLAY_C',
                      'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_INPLAY_L','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_INPLAY_C','STDEV_BET_SIZE_OUTPLAY_C',
                      'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY',
                      'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY',
                      'AVG_TAKEN_HOUR_INPLAY','AVG_TAKEN_HOUR_OUTPLAY',
                      'PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE',
                      'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY',
                      'MARGIN_INPLAY','MARGIN_OUTPLAY'
)
test_dt[, -c(1,2,33:41)][is.na(test_dt[, -c(1,2,33:41)])] <- 0
test_dt[, c(33:36)][is.na(test_dt[, c(33:36)])] <- 0
test_dt[, c(39:41)][is.na(test_dt[, c(39:41)])] <- 0.5
test_dt$AVG_TAKEN_HOUR_INPLAY <- floor(test_dt$AVG_TAKEN_HOUR_INPLAY)
test_dt$AVG_TAKEN_HOUR_OUTPLAY <- floor(test_dt$AVG_TAKEN_HOUR_OUTPLAY)

# 10. CANCEL_RATIO
test_dt$CANCEL_RATIO_INPLAY <- test_dt$TRANSACTION_COUNT_INPLAY_C/(test_dt$TRANSACTION_COUNT_INPLAY_C+test_dt$TRANSACTION_COUNT_INPLAY+test_dt$TRANSACTION_COUNT_INPLAY_L)
test_dt$CANCEL_RATIO_OUTPLAY <- test_dt$TRANSACTION_COUNT_OUTPLAY_C/(test_dt$TRANSACTION_COUNT_OUTPLAY_C+test_dt$TRANSACTION_COUNT_OUTPLAY+test_dt$TRANSACTION_COUNT_OUTPLAY_L)

# 11. INPLAY_RATIO
test_dt$INPLAY_RATIO <- test_dt$TRANSACTION_COUNT_INPLAY/(test_dt$TRANSACTION_COUNT_INPLAY + test_dt$TRANSACTION_COUNT_OUTPLAY)
test_dt[,c(46:48)][is.na(test_dt[,c(46:48)])] <- 0
