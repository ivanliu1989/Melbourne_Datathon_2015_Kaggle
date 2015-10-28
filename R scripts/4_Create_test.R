setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/train_validation.RData');
load('data/mbr_event_data.RData');ls()

submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
head(submit)
head(test)

### Add Features to Test
names(test) <- c('ACCOUNT_ID', names(test)[-1])
test$TOT_BET_SIZE <- test$TRANSACTION_COUNT * test$AVG_BET_SIZE
test$STDEV_BET_SIZE_N <- test$AVG_BET_SIZE + test$STDEV_BET_SIZE
head(test)

# TRANSACTION_COUNT
TRANSACTION_COUNT <- aggregate(TRANSACTION_COUNT ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, sum, na.rm=T) 
# AVG_BET_SIZE
TOT_BET_SIZE <- aggregate(TOT_BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, sum, na.rm=T)  #?
TOT_BET_SIZE$AVG_BET_SIZE <- TOT_BET_SIZE$TOT_BET_SIZE / TRANSACTION_COUNT$TRANSACTION_COUNT #?
AVG_BET_SIZE <- TOT_BET_SIZE[, -5]
# MAX_BET_SIZE
MAX_BET_SIZE <- aggregate(MAX_BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, max, na.rm=T) 
# MIN_BET_SIZE
MIN_BET_SIZE <- aggregate(MIN_BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, min, na.rm=T) 
# STDEV_BET_SIZE
test <- merge(test, AVG_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test$VAR <- ((test$AVG_BET_SIZE.y - test$STDEV_BET_SIZE_N)**2) * test$TRANSACTION_COUNT
VAR <- aggregate(VAR ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, sum)
VAR$VAR <- VAR$VAR / TRANSACTION_COUNT$TRANSACTION_COUNT
VAR$STDEV_BET_SIZE <- VAR$VAR ** 0.5
VAR$VAR <- NULL
# Final Bonus Feature
test_clean <- test[!duplicated(test[,c(1:4)]),c(1:4)]
test_clean <- merge(test_clean, TRANSACTION_COUNT, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, AVG_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, MAX_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, MIN_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, VAR, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))

test <- test_clean
test_dt <- test[!duplicated(test[,c(1,2)]),c(1,2)]
  
### MERGE AND RETURN Member base
# Event specific
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

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

names(test_dt) <- c('ACCOUNT_ID','EVENT_ID','TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_INPLAY_L','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_INPLAY_C','TRANSACTION_COUNT_OUTPLAY_C',
                    'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_INPLAY_L','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_INPLAY_C','AVG_BET_SIZE_OUTPLAY_C',
                    'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_INPLAY_L','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_INPLAY_C','MAX_BET_SIZE_OUTPLAY_C',
                    'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_INPLAY_L','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_INPLAY_C','MIN_BET_SIZE_OUTPLAY_C',
                    'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_INPLAY_L','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_INPLAY_C','STDEV_BET_SIZE_OUTPLAY_C')
head(test_dt)

# Overall
AVG_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,33)]),c(1,33)]
AVG_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,34)]),c(1,34)]
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

STDEV_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,35)]),c(1,35)]
STDEV_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,36)]),c(1,36)]
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

AVG_TAKEN_HOUR_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,37)]),c(1,37)]
AVG_TAKEN_HOUR_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,38)]),c(1,38)]
test_dt <- merge(test_dt, AVG_TAKEN_HOUR_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_TAKEN_HOUR_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,39)]),c(1,39)]
PREV_WIN_RATE_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,40)]),c(1,40)]
test_dt <- merge(test_dt, PREV_WIN_RATE_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, PREV_WIN_RATE_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE <- mbr.event[!duplicated(mbr.event[,c(1,41)]),c(1,41)]
test_dt <- merge(test_dt, PREV_WIN_RATE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

NET_PROFIT_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,42)]),c(1,42)]
NET_PROFIT_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,43)]),c(1,43)]
test_dt <- merge(test_dt, NET_PROFIT_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, NET_PROFIT_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

MARGIN_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,44)]),c(1,44)]
MARGIN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,45)]),c(1,45)]
test_dt <- merge(test_dt, MARGIN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MARGIN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

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
test <- test_dt
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

#META DATA
# 1. RFM

# 2. CLUSTERING

save(test_dt, file='data/test.RData')

