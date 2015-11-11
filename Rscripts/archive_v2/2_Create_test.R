setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/1_complete_data.RData');ls()

mbr.event <- total
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
head(submit);head(test)

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
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'L', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
# test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'C', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

names(test_dt) <- c('ACCOUNT_ID','EVENT_ID',
                    'TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
                    'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
                    'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
                    'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
                    'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C')
head(test_dt)

# Overall
AVG_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_INPLAY')]
AVG_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_OUTPLAY')]
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

STDEV_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_INPLAY')]
STDEV_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_OUTPLAY')]
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

SKEW_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','SKEW_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','SKEW_PLACED_TAKEN_TIME_INPLAY')]
SKEW_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','SKEW_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','SKEW_PLACED_TAKEN_TIME_OUTPLAY')]
test_dt <- merge(test_dt, SKEW_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, SKEW_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

KURT_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','KURT_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','KURT_PLACED_TAKEN_TIME_INPLAY')]
KURT_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','KURT_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','KURT_PLACED_TAKEN_TIME_OUTPLAY')]
test_dt <- merge(test_dt, KURT_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, KURT_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

AVG_TAKEN_HOUR_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_TAKEN_HOUR_INPLAY')]),c('ACCOUNT_ID','AVG_TAKEN_HOUR_INPLAY')]
AVG_TAKEN_HOUR_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_TAKEN_HOUR_OUTPLAY')]),c('ACCOUNT_ID','AVG_TAKEN_HOUR_OUTPLAY')]
test_dt <- merge(test_dt, AVG_TAKEN_HOUR_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_TAKEN_HOUR_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

STDEV_TAKEN_HOUR_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_TAKEN_HOUR_INPLAY')]),c('ACCOUNT_ID','STDEV_TAKEN_HOUR_INPLAY')]
STDEV_TAKEN_HOUR_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_TAKEN_HOUR_OUTPLAY')]),c('ACCOUNT_ID','STDEV_TAKEN_HOUR_OUTPLAY')]
test_dt <- merge(test_dt, STDEV_TAKEN_HOUR_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_TAKEN_HOUR_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','PREV_WIN_RATE_INPLAY')]),c('ACCOUNT_ID','PREV_WIN_RATE_INPLAY')]
PREV_WIN_RATE_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','PREV_WIN_RATE_OUTPLAY')]),c('ACCOUNT_ID','PREV_WIN_RATE_OUTPLAY')]
test_dt <- merge(test_dt, PREV_WIN_RATE_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, PREV_WIN_RATE_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','PREV_WIN_RATE')]),c('ACCOUNT_ID','PREV_WIN_RATE')]
test_dt <- merge(test_dt, PREV_WIN_RATE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

NET_PROFIT_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','NET_PROFIT_INPLAY')]),c('ACCOUNT_ID','NET_PROFIT_INPLAY')]
NET_PROFIT_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','NET_PROFIT_OUTPLAY')]),c('ACCOUNT_ID','NET_PROFIT_OUTPLAY')]
test_dt <- merge(test_dt, NET_PROFIT_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, NET_PROFIT_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

MARGIN_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MARGIN_INPLAY')]),c('ACCOUNT_ID','MARGIN_INPLAY')]
MARGIN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MARGIN_OUTPLAY')]),c('ACCOUNT_ID','MARGIN_OUTPLAY')]
test_dt <- merge(test_dt, MARGIN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MARGIN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

names(test_dt) <- c('ACCOUNT_ID','EVENT_ID',
                      'TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
                      'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
                      'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
                      'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
                      'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C',
                      
                      'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY',
                      'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY',
                      'SKEW_PLACED_TAKEN_TIME_INPLAY','SKEW_PLACED_TAKEN_TIME_OUTPLAY',
                      'KURT_PLACED_TAKEN_TIME_INPLAY','KURT_PLACED_TAKEN_TIME_OUTPLAY',
                      'AVG_TAKEN_HOUR_INPLAY','AVG_TAKEN_HOUR_OUTPLAY',
                      'STDEV_TAKEN_HOUR_INPLAY','STDEV_TAKEN_HOUR_OUTPLAY',
                      'PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE',
                      'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY',
                      'MARGIN_INPLAY','MARGIN_OUTPLAY'
)
test <- test_dt
test_dt[, c('TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
              'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
              'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
              'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
              'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C'
#               ,'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY',
#               'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY',
#               'SKEW_PLACED_TAKEN_TIME_INPLAY','SKEW_PLACED_TAKEN_TIME_OUTPLAY',
#               'KURT_PLACED_TAKEN_TIME_INPLAY','KURT_PLACED_TAKEN_TIME_OUTPLAY',
#               'STDEV_TAKEN_HOUR_INPLAY','STDEV_TAKEN_HOUR_OUTPLAY',
#               'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY',
#               'MARGIN_INPLAY','MARGIN_OUTPLAY'
            )][is.na(test_dt[, c('TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
                                                                     'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
                                                                     'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
                                                                     'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
                                                                     'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C'
#                                                                     ,'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY',
#                                                                      'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY',
#                                                                      'SKEW_PLACED_TAKEN_TIME_INPLAY','SKEW_PLACED_TAKEN_TIME_OUTPLAY',
#                                                                      'KURT_PLACED_TAKEN_TIME_INPLAY','KURT_PLACED_TAKEN_TIME_OUTPLAY',
#                                                                      'STDEV_TAKEN_HOUR_INPLAY','STDEV_TAKEN_HOUR_OUTPLAY',
#                                                                      'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY',
#                                                                      'MARGIN_INPLAY','MARGIN_OUTPLAY'
                                 )])] <- 0
# test_dt[, c('PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE')][is.na(test_dt[, c('PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE')])] <- 0.5

head(test_dt)
# 10. CANCEL_RATIO
test_dt$CANCEL_RATIO_OUTPLAY <- test_dt$TRANSACTION_COUNT_OUTPLAY_C/(test_dt$TRANSACTION_COUNT_OUTPLAY_C+test_dt$TRANSACTION_COUNT_OUTPLAY+test_dt$TRANSACTION_COUNT_OUTPLAY_L)

# 11. INPLAY_RATIO
test_dt$INPLAY_RATIO <- test_dt$TRANSACTION_COUNT_INPLAY/(test_dt$TRANSACTION_COUNT_INPLAY + test_dt$TRANSACTION_COUNT_OUTPLAY)
test_dt[,c(42:43)][is.na(test_dt[,c(42:43)])] <- 0

# 12. Country
country <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID', 'COUNTRY_OF_RESIDENCE_NAME')]),c('ACCOUNT_ID', 'COUNTRY_OF_RESIDENCE_NAME')]
test_dt <- merge(test_dt, country, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

# 13. BL_RATIO
BL_RATIO_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID', 'BL_RATIO_INPLAY')]),c('ACCOUNT_ID', 'BL_RATIO_INPLAY')]
test_dt <- merge(test_dt, BL_RATIO_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
BL_RATIO_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID', 'BL_RATIO_OUTPLAY')]),c('ACCOUNT_ID', 'BL_RATIO_OUTPLAY')]
test_dt <- merge(test_dt, BL_RATIO_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
BL_RATIO <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID', 'BL_RATIO')]),c('ACCOUNT_ID', 'BL_RATIO')]
test_dt <- merge(test_dt, BL_RATIO, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

# 14. B_L_DIFF
    test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
    test <- test[test$STATUS_ID == 'S',]
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
    
    ### test_feat merge
    test_feat_inplay <- test_feat_final[test_feat_final$INPLAY_BET == 'Y',-c(3,4,10)]; 
    names(test_feat_inplay) <- c('ACCOUNT_ID', 'EVENT_ID', 'BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                                 'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN')
    test_feat_outplay <- test_feat_final[!test_feat_final$INPLAY_BET == 'Y',-c(3,4,10)]
    names(test_feat_outplay) <- c('ACCOUNT_ID', 'EVENT_ID', 'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                                  'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')
    test_dt <- merge(test_dt, test_feat_inplay, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    test_dt <- merge(test_dt, test_feat_outplay, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    test_dt[,c('BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
            'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
            'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
            'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')][is.na(test_dt[,c('BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                                                                                                               'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
                                                                                                               'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                                                                                                               'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')])] <- 0
    
#META DATA
# 1. RFM

# 2. CLUSTERING

# Imputation
# test_dt[is.na(test_dt$AVG_TAKEN_HOUR_INPLAY),'AVG_TAKEN_HOUR_INPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
# test_dt[is.na(test_dt$AVG_TAKEN_HOUR_OUTPLAY),'AVG_TAKEN_HOUR_OUTPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)
test <- test_dt

apply(test,2, function(x) mean(is.na(x)))
save(test, file='data/2_test.RData')

