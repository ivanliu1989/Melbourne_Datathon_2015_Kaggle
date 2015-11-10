setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/S_complete_data_clean_up.RData');
# load('../Datathon_Full_Dataset/cleaned_raw_data.RData')

mbr.event <- total
test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")

### Add Features to Test
names(test) <- c('ACCOUNT_ID', names(test)[-1])
test$TOT_BET_SIZE <- test$TRANSACTION_COUNT * test$AVG_BET_SIZE
test$STDEV_BET_SIZE_N <- test$AVG_BET_SIZE + test$STDEV_BET_SIZE
head(test)


# TRANSACTION_COUNT
TRANSACTION_COUNT <- aggregate(TRANSACTION_COUNT ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, sum, na.rm=T) 
TRANSACTION_COUNT_ALL <- aggregate(TRANSACTION_COUNT ~ ACCOUNT_ID + EVENT_ID + STATUS_ID, data=test, sum, na.rm=T) 

# AVG_BET_SIZE
TOT_BET_SIZE <- aggregate(TOT_BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=test, sum, na.rm=T)  #?
TOT_BET_SIZE$AVG_BET_SIZE <- TOT_BET_SIZE$TOT_BET_SIZE / TRANSACTION_COUNT$TRANSACTION_COUNT #?
AVG_BET_SIZE <- TOT_BET_SIZE[, -5]
TOT_BET_SIZE_ALL <- aggregate(TOT_BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID ,data=test, sum, na.rm=T)  #?
TOT_BET_SIZE_ALL$AVG_BET_SIZE_ALL <- TOT_BET_SIZE_ALL$TOT_BET_SIZE / TRANSACTION_COUNT_ALL$TRANSACTION_COUNT #?
AVG_BET_SIZE_ALL <- TOT_BET_SIZE_ALL[, -5]

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
VAR_ALL <- aggregate(VAR ~ ACCOUNT_ID + EVENT_ID + STATUS_ID ,data=test, sum)
VAR_ALL$VAR <- VAR_ALL$VAR / TRANSACTION_COUNT_ALL$TRANSACTION_COUNT
VAR_ALL$STDEV_BET_SIZE <- VAR_ALL$VAR ** 0.5
VAR_ALL$VAR <- NULL

# Final Bonus Feature
test_clean <- test[!duplicated(test[,c(1:4)]),c(1:4)]
test_clean <- merge(test_clean, TRANSACTION_COUNT, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, TRANSACTION_COUNT_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID'))

test_clean <- merge(test_clean, AVG_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, AVG_BET_SIZE_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID'))

test_clean <- merge(test_clean, MAX_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, MIN_BET_SIZE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, VAR, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
test_clean <- merge(test_clean, VAR_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID'))

test <- test_clean

# START  **********
test_dt <- test[!duplicated(test[,c(1,2)]),c(1,2)]

### MERGE AND RETURN Member base
# Event specific
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT.x')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT.x')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT.y')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'TOT_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))

test_dt <- merge(test_dt, test[test$INPLAY_BET == 'Y' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE.x')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE.x')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
test_dt <- merge(test_dt, test[test$INPLAY_BET == 'N' & test$STATUS_ID == 'S', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE.y')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))


names(test_dt) <- c('ACCOUNT_ID','EVENT_ID',
                    'TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_ALL',
                    'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_ALL',
                    'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY',
                    'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY',
                    'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_ALL')
head(test_dt)

# Overall
AVG_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_INPLAY')]
AVG_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_OUTPLAY')]
AVG_PLACED_TAKEN_TIME_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_ALL')]),c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME_ALL')]
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_PLACED_TAKEN_TIME_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

MEDIAN_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MEDIAN_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','MEDIAN_PLACED_TAKEN_TIME_INPLAY')]
MEDIAN_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MEDIAN_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','MEDIAN_PLACED_TAKEN_TIME_OUTPLAY')]
MEDIAN_PLACED_TAKEN_TIME_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MEDIAN_PLACED_TAKEN_TIME_ALL')]),c('ACCOUNT_ID','MEDIAN_PLACED_TAKEN_TIME_ALL')]
test_dt <- merge(test_dt, MEDIAN_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MEDIAN_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MEDIAN_PLACED_TAKEN_TIME_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

STDEV_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_INPLAY')]),c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_INPLAY')]
STDEV_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_OUTPLAY')]),c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_OUTPLAY')]
STDEV_PLACED_TAKEN_TIME_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_ALL')]),c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME_ALL')]
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_PLACED_TAKEN_TIME_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

STDEV_TAKEN_HOUR_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_TAKEN_HOUR_INPLAY')]),c('ACCOUNT_ID','STDEV_TAKEN_HOUR_INPLAY')]
STDEV_TAKEN_HOUR_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','STDEV_TAKEN_HOUR_OUTPLAY')]),c('ACCOUNT_ID','STDEV_TAKEN_HOUR_OUTPLAY')]
test_dt <- merge(test_dt, STDEV_TAKEN_HOUR_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_TAKEN_HOUR_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','PREV_WIN_RATE_INPLAY')]),c('ACCOUNT_ID','PREV_WIN_RATE_INPLAY')]
PREV_WIN_RATE_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','PREV_WIN_RATE_OUTPLAY')]),c('ACCOUNT_ID','PREV_WIN_RATE_OUTPLAY')]
PREV_WIN_RATE_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','PREV_WIN_RATE_ALL')]),c('ACCOUNT_ID','PREV_WIN_RATE_ALL')]
test_dt <- merge(test_dt, PREV_WIN_RATE_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, PREV_WIN_RATE_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, PREV_WIN_RATE_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

NET_PROFIT_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','NET_PROFIT_INPLAY')]),c('ACCOUNT_ID','NET_PROFIT_INPLAY')]
NET_PROFIT_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','NET_PROFIT_OUTPLAY')]),c('ACCOUNT_ID','NET_PROFIT_OUTPLAY')]
NET_PROFIT_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','NET_PROFIT_ALL')]),c('ACCOUNT_ID','NET_PROFIT_ALL')]
test_dt <- merge(test_dt, NET_PROFIT_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, NET_PROFIT_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, NET_PROFIT_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

MARGIN_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MARGIN_INPLAY')]),c('ACCOUNT_ID','MARGIN_INPLAY')]
MARGIN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MARGIN_OUTPLAY')]),c('ACCOUNT_ID','MARGIN_OUTPLAY')]
MARGIN_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MARGIN_ALL')]),c('ACCOUNT_ID','MARGIN_ALL')]
test_dt <- merge(test_dt, MARGIN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MARGIN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MARGIN_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

SD_BET_TAKEN_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','SD_BET_TAKEN_INPLAY')]),c('ACCOUNT_ID','SD_BET_TAKEN_INPLAY')]
SD_BET_TAKEN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','SD_BET_TAKEN_OUTPLAY')]),c('ACCOUNT_ID','SD_BET_TAKEN_OUTPLAY')]
SD_BET_TAKEN_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','SD_BET_TAKEN_ALL')]),c('ACCOUNT_ID','SD_BET_TAKEN_ALL')]
test_dt <- merge(test_dt, SD_BET_TAKEN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, SD_BET_TAKEN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, SD_BET_TAKEN_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

AVG_BET_TAKEN_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_BET_TAKEN_INPLAY')]),c('ACCOUNT_ID','AVG_BET_TAKEN_INPLAY')]
AVG_BET_TAKEN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_BET_TAKEN_OUTPLAY')]),c('ACCOUNT_ID','AVG_BET_TAKEN_OUTPLAY')]
AVG_BET_TAKEN_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','AVG_BET_TAKEN_ALL')]),c('ACCOUNT_ID','AVG_BET_TAKEN_ALL')]
test_dt <- merge(test_dt, AVG_BET_TAKEN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_BET_TAKEN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_BET_TAKEN_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

MEDIAN_BET_TAKEN_INPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MEDIAN_BET_TAKEN_INPLAY')]),c('ACCOUNT_ID','MEDIAN_BET_TAKEN_INPLAY')]
MEDIAN_BET_TAKEN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MEDIAN_BET_TAKEN_OUTPLAY')]),c('ACCOUNT_ID','MEDIAN_BET_TAKEN_OUTPLAY')]
MEDIAN_BET_TAKEN_ALL <- mbr.event[!duplicated(mbr.event[,c('ACCOUNT_ID','MEDIAN_BET_TAKEN_ALL')]),c('ACCOUNT_ID','MEDIAN_BET_TAKEN_ALL')]
test_dt <- merge(test_dt, MEDIAN_BET_TAKEN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MEDIAN_BET_TAKEN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MEDIAN_BET_TAKEN_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

names(test_dt) <- c('ACCOUNT_ID','EVENT_ID',
                    'TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_ALL',
                    'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_ALL',
                    'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY',
                    'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY',
                    'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_ALL',
                    
                    'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY','AVG_PLACED_TAKEN_TIME_ALL',
                    'MEDIAN_PLACED_TAKEN_TIME_INPLAY','MEDIAN_PLACED_TAKEN_TIME_OUTPLAY','MEDIAN_PLACED_TAKEN_TIME_ALL',
                    'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY','STDEV_PLACED_TAKEN_TIME_ALL',
                    'STDEV_TAKEN_HOUR_INPLAY','STDEV_TAKEN_HOUR_OUTPLAY',
                    'PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE_ALL',
                    'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY','NET_PROFIT_ALL',
                    'MARGIN_INPLAY','MARGIN_OUTPLAY','MARGIN_ALL',
                    'SD_BET_TAKEN_INPLAY','SD_BET_TAKEN_OUTPLAY','SD_BET_TAKEN_ALL',
                    'AVG_BET_TAKEN_INPLAY','AVG_BET_TAKEN_OUTPLAY','AVG_BET_TAKEN_ALL',
                    'MEDIAN_BET_TAKEN_INPLAY','MEDIAN_BET_TAKEN_OUTPLAY','MEDIAN_BET_TAKEN_ALL'
)
test <- test_dt
test_dt[, c(3:44)][is.na(test_dt[, c(3:44)])] <- 0

head(test_dt)

# 11. INPLAY_RATIO
test_dt$INPLAY_RATIO <- test_dt$TRANSACTION_COUNT_INPLAY/(test_dt$TRANSACTION_COUNT_INPLAY + test_dt$TRANSACTION_COUNT_OUTPLAY)
test_dt[,45][is.na(test_dt[,45])] <- 0

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

test <- test_dt

apply(test,2, function(x) mean(is.na(x)))
save(test, file='data/S_complete_data_clean_up_test.RData')

