setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/train_validation.RData');ls()

mbr.event <- total
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

SKEW_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,37)]),c(1,37)]
SKEW_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,38)]),c(1,38)]
test_dt <- merge(test_dt, SKEW_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, SKEW_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

KURT_PLACED_TAKEN_TIME_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,39)]),c(1,39)]
KURT_PLACED_TAKEN_TIME_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,40)]),c(1,40)]
test_dt <- merge(test_dt, KURT_PLACED_TAKEN_TIME_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, KURT_PLACED_TAKEN_TIME_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

AVG_TAKEN_HOUR_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,41)]),c(1,41)]
AVG_TAKEN_HOUR_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,42)]),c(1,42)]
test_dt <- merge(test_dt, AVG_TAKEN_HOUR_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, AVG_TAKEN_HOUR_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

STDEV_TAKEN_HOUR_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,43)]),c(1,43)]
STDEV_TAKEN_HOUR_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,44)]),c(1,44)]
test_dt <- merge(test_dt, STDEV_TAKEN_HOUR_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, STDEV_TAKEN_HOUR_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,45)]),c(1,45)]
PREV_WIN_RATE_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,46)]),c(1,46)]
test_dt <- merge(test_dt, PREV_WIN_RATE_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, PREV_WIN_RATE_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

PREV_WIN_RATE <- mbr.event[!duplicated(mbr.event[,c(1,47)]),c(1,47)]
test_dt <- merge(test_dt, PREV_WIN_RATE, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

NET_PROFIT_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,48)]),c(1,48)]
NET_PROFIT_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,49)]),c(1,49)]
test_dt <- merge(test_dt, NET_PROFIT_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, NET_PROFIT_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

MARGIN_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,50)]),c(1,50)]
MARGIN_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,51)]),c(1,51)]
test_dt <- merge(test_dt, MARGIN_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test_dt <- merge(test_dt, MARGIN_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

names(mbr.event) <- c('ACCOUNT_ID','EVENT_ID','TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_INPLAY_L','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_INPLAY_C','TRANSACTION_COUNT_OUTPLAY_C',
                      'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_INPLAY_L','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_INPLAY_C','AVG_BET_SIZE_OUTPLAY_C',
                      'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_INPLAY_L','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_INPLAY_C','MAX_BET_SIZE_OUTPLAY_C',
                      'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_INPLAY_L','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_INPLAY_C','MIN_BET_SIZE_OUTPLAY_C',
                      'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_INPLAY_L','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_INPLAY_C','STDEV_BET_SIZE_OUTPLAY_C',
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
test_dt[, -c(1,2,45:47)][is.na(test_dt[, -c(1,2,45:47)])] <- 0
test_dt[, c(45:47)][is.na(test_dt[, c(45:47)])] <- 0.5
test_dt$AVG_TAKEN_HOUR_INPLAY <- floor(test_dt$AVG_TAKEN_HOUR_INPLAY)
test_dt$AVG_TAKEN_HOUR_OUTPLAY <- floor(test_dt$AVG_TAKEN_HOUR_OUTPLAY)

head(test_dt)
# 10. CANCEL_RATIO
test_dt$CANCEL_RATIO_INPLAY <- test_dt$TRANSACTION_COUNT_INPLAY_C/(test_dt$TRANSACTION_COUNT_INPLAY_C+test_dt$TRANSACTION_COUNT_INPLAY+test_dt$TRANSACTION_COUNT_INPLAY_L)
test_dt$CANCEL_RATIO_OUTPLAY <- test_dt$TRANSACTION_COUNT_OUTPLAY_C/(test_dt$TRANSACTION_COUNT_OUTPLAY_C+test_dt$TRANSACTION_COUNT_OUTPLAY+test_dt$TRANSACTION_COUNT_OUTPLAY_L)

# 11. INPLAY_RATIO
test_dt$INPLAY_RATIO <- test_dt$TRANSACTION_COUNT_INPLAY/(test_dt$TRANSACTION_COUNT_INPLAY + test_dt$TRANSACTION_COUNT_OUTPLAY)
test_dt[,c(52:54)][is.na(test_dt[,c(52:54)])] <- 0

# 12. Country
country <- mbr.event[!duplicated(mbr.event[,c(1,55)]),c(1,55)]
test_dt <- merge(test_dt, country, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

# 13. BL_RATIO
BL_RATIO_INPLAY <- mbr.event[!duplicated(mbr.event[,c(1,56)]),c(1,56)]
test_dt <- merge(test_dt, BL_RATIO_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
BL_RATIO_OUTPLAY <- mbr.event[!duplicated(mbr.event[,c(1,57)]),c(1,57)]
test_dt <- merge(test_dt, BL_RATIO_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
BL_RATIO <- mbr.event[!duplicated(mbr.event[,c(1,58)]),c(1,58)]
test_dt <- merge(test_dt, BL_RATIO, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

#META DATA
# 1. RFM

# 2. CLUSTERING


# Imputation
test_dt[is.na(test_dt$AVG_TAKEN_HOUR_INPLAY),'AVG_TAKEN_HOUR_INPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
test_dt[is.na(test_dt$AVG_TAKEN_HOUR_OUTPLAY),'AVG_TAKEN_HOUR_OUTPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)
test <- test_dt

apply(test,2, function(x) mean(is.na(x)))
save(test, file='data/test.RData')

save(train, validation, test, total, file='data/train_validation_test.RData')

write.csv(total,'../total.csv',quote = FALSE,row.names = FALSE)
write.csv(test,'../test.csv',quote = FALSE,row.names = FALSE)

t_c <- unique(test$ACCOUNT_ID)
tot_c <- unique(total$ACCOUNT_ID)
ex <- t_c[!t_c %in% tot_c]
