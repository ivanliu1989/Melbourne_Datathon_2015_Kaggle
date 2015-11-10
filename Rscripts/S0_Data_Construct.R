setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(MASS);library(e1071)

### Read Data
load('../Datathon_Full_Dataset/cleaned_raw_data.RData')
ls()

### Feature generating
source('Rscripts/S0.5_functions.r')
total <- feat.eng(dt)
total <- mbr.event 
apply(total,2, function(x) mean(is.na(x)))
apply(total,2, function(x) length(unique(x)))

### output
save(total, file='data/1_complete_data_new.RData')
save(total, file='data/S_complete_data_clean_up.RData')

### new features
library(dplyr); library(tidyr)
PREV_FREQ <- tbl_df(read.csv('New features/PREV_FREQ.csv'))
PREV_FREQ_SKEW <- tbl_df(read.csv('New features/PREV_FREQ_SKEW.csv'))
PREV_WIN <- tbl_df(read.csv('New features/PREV_WIN.csv'))
PREV_WIN_RATE <- tbl_df(read.csv('New features/PREV_WIN_RATE.csv'))
PREV_WIN_SKEW <- tbl_df(read.csv('New features/PREV_WIN_SKEW.csv'))
MARGIN_SKEW <- tbl_df(read.csv('New features/MARGIN_SKEW.csv'))
MARGIN_TOTAL <- tbl_df(read.csv('New features/TOTAL_MARGIN.csv'))

PREV_FREQ <- gather(PREV_FREQ, ACCOUNT_ID); names(PREV_FREQ) <- c('ACCOUNT_ID', 'EVENT_ID', 'PREV_FREQ')
PREV_FREQ_SKEW <- gather(PREV_FREQ_SKEW, ACCOUNT_ID); names(PREV_FREQ_SKEW) <- c('ACCOUNT_ID', 'EVENT_ID', 'PREV_FREQ_SKEW')
PREV_WIN <- gather(PREV_WIN, ACCOUNT_ID); names(PREV_WIN) <- c('ACCOUNT_ID', 'EVENT_ID', 'PREV_WIN')
PREV_WIN_RATE <- gather(PREV_WIN_RATE, ACCOUNT_ID); names(PREV_WIN_RATE) <- c('ACCOUNT_ID', 'EVENT_ID', 'PREV_WIN_RATE')
PREV_WIN_SKEW <- gather(PREV_WIN_SKEW, ACCOUNT_ID); names(PREV_WIN_SKEW) <- c('ACCOUNT_ID', 'EVENT_ID', 'PREV_WIN_SKEW')
MARGIN_SKEW <- gather(MARGIN_SKEW, Row.Labels); names(MARGIN_SKEW) <- c('ACCOUNT_ID', 'EVENT_ID', 'MARGIN_SKEW')
MARGIN_TOTAL <- gather(MARGIN_TOTAL, Row.Labels); names(MARGIN_TOTAL) <- c('ACCOUNT_ID', 'EVENT_ID', 'MARGIN_TOTAL')
dim(PREV_WIN_SKEW); dim(PREV_WIN_RATE); dim(PREV_WIN); dim(PREV_FREQ_SKEW); dim(PREV_FREQ); dim(MARGIN_SKEW); dim(MARGIN_TOTAL)

NEW_FEATURE <- merge(PREV_FREQ, PREV_FREQ_SKEW, all.x = TRUE, all.y = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
NEW_FEATURE <- merge(NEW_FEATURE, PREV_WIN, all.x = TRUE, all.y = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
NEW_FEATURE <- merge(NEW_FEATURE, PREV_WIN_RATE, all.x = TRUE, all.y = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
NEW_FEATURE <- merge(NEW_FEATURE, PREV_WIN_SKEW, all.x = TRUE, all.y = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
NEW_FEATURE <- merge(NEW_FEATURE, MARGIN_SKEW, all.x = TRUE, all.y = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
NEW_FEATURE <- merge(NEW_FEATURE, MARGIN_TOTAL, all.x = TRUE, all.y = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
dim(NEW_FEATURE); head(NEW_FEATURE)

NEW_FEATURE$EVENT_ID <- gsub('X', '',NEW_FEATURE$EVENT_ID)
NEW_FEATURE_train <- NEW_FEATURE[NEW_FEATURE$EVENT_ID != 'test',]
NEW_FEATURE_test <- NEW_FEATURE[NEW_FEATURE$EVENT_ID == 'test',]
table(NEW_FEATURE_train$EVENT_ID)

save(NEW_FEATURE_train, NEW_FEATURE_test, file='data/NEW_FEATURE.RData')


### output full data matrix
flag <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + OFF_DT, data=d, sum, na.rm = T) 
write.csv(flag, 'INVEST_PIVOT.csv')

test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
test <- test[test$STATUS_ID == 'S', ]
test$INVEST <- test$AVG_BET_SIZE * test$TRANSACTION_COUNT
test$OFF_DT <-  '01/12/2015 3:30:01 AM'
flag <- aggregate(INVEST ~ Account_ID + EVENT_ID + OFF_DT, data=test, sum, na.rm = T) 
write.csv(flag, 'INVEST_PIVOT_test.csv')

# profit
flag <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + OFF_DT, data=d, sum, na.rm = T) 
write.csv(flag, 'PROFIT_LOSS_PIVOT.csv')
