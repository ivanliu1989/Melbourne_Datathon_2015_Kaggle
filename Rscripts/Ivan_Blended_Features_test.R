rm(list=ls());gc()
load('data/train_20151115.RData')
test <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
names(test) <- c('ACCOUNT_ID',names(test)[-1])
d <- test
####################
# Function #########
####################
shift<-function(x,fill,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
    
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
    
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(fill,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(fill,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}

#######################
# Pre process #########
#######################
#rows of interest - just the settled bets
requiredRows <- which(d$STATUS_ID == 'S')
requiredRows2 <- which(d$STATUS_ID == 'C')
#filter
dc <- d[requiredRows2,]
d <- d[requiredRows,]
dc_raw <- dc
d_raw <- d

#############################    
# Feature engineering #######
#############################
library(data.table)
d <- data.table(d)
d_c <- data.table(dc)
train <- data.table(train)
# 1. Count/Bet features
load(file='test_sd_bet_size.RData'); ls()
    d[,BET_SIZE:=TRANSACTION_COUNT * AVG_BET_SIZE, by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,TRANSACTION_COUNT:=sum(TRANSACTION_COUNT), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,AVG_BET_SIZE:=BET_SIZE/TRANSACTION_COUNT, by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,MAX_BET_SIZE:=max(MAX_BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,MIN_BET_SIZE:=min(MIN_BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,TOTAL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_1 <- d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, AVG_BET_SIZE, MAX_BET_SIZE, MIN_BET_SIZE, TOTAL_BET_SIZE)]
    feat_1 <- unique(feat_1)
    feat_1 <- merge(feat_1, as.data.table(sd_bet_size[,-3]), all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
feat_1 <- unique(feat_1[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, AVG_BET_SIZE, MAX_BET_SIZE, MIN_BET_SIZE, STDEV_BET_SIZE, TOTAL_BET_SIZE)])

    d_c[,BET_SIZE:=TRANSACTION_COUNT * AVG_BET_SIZE, by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d_c[,TRANSACTION_COUNT_C:=sum(TRANSACTION_COUNT), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d_c[,AVG_BET_SIZE_C:=BET_SIZE/TRANSACTION_COUNT, by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d_c[,MAX_BET_SIZE_C:=max(MAX_BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d_c[,MIN_BET_SIZE_C:=min(MIN_BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d_c[,TOTAL_BET_SIZE_C:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_1_c <- d_c[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT_C, AVG_BET_SIZE_C, MAX_BET_SIZE_C, MIN_BET_SIZE_C, TOTAL_BET_SIZE_C)]
    feat_1_c <- unique(feat_1_c)
    names(sd_bet_size_c) <- c(names(sd_bet_size_c)[-4], 'STDEV_BET_SIZE_C')
    feat_1_c <- merge(feat_1_c, as.data.table(sd_bet_size_c[,-3]), all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
feat_1_c <- unique(feat_1_c[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT_C, AVG_BET_SIZE_C, MAX_BET_SIZE_C, MIN_BET_SIZE_C, STDEV_BET_SIZE_C, TOTAL_BET_SIZE_C)])

feat_1 <- merge(feat_1, feat_1_c, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))

# 2. Taken time / placed hour
    feat_2 <- train[,.(ACCOUNT_ID, SD_PLACED_TAKEN_TIME, PERC_PLACED_TAKEN_TIME_1, PERC_PLACED_TAKEN_TIME_2, PERC_PLACED_TAKEN_TIME_3, PERC_PLACED_TAKEN_TIME_4, PERC_PLACED_TAKEN_TIME_5,
                   SD_PLACED_HOUR, AVG_PLACED_HOUR)]
feat_2 <- unique(feat_2)

# 3. Win hist / profit / margin / Experience / frequency
feat_3 <- unique(train[,.(ACCOUNT_ID, EXPERIENCE, WIN_HIST, MARGIN, SD_MARGIN_EVENT, AVG_MARGIN_EVENT, WIN_RATE, FREQUENCY)])

# 4. Inplay/Outplay | Bet/lay rate | Country ratio
    feat_4 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TOTAL_BET_SIZE)])
    feat_4 <- unique(feat_4[,TOTAL_BET_SIZE:= sum(TOTAL_BET_SIZE), by=c('ACCOUNT_ID', 'EVENT_ID')])
    feat_4_2 <- unique(d[INPLAY_BET=='Y',.(ACCOUNT_ID, EVENT_ID,BET_SIZE)])
    feat_4_2 <- unique(feat_4_2[,BET_SIZE:= sum(BET_SIZE), by=c('ACCOUNT_ID', 'EVENT_ID')])
    feat_4 <- merge(feat_4,feat_4_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
    feat_4[,INPLAY_OUTPLAY_RATIO:=ifelse(is.na(BET_SIZE),0,BET_SIZE)/TOTAL_BET_SIZE]
feat_4 <- unique(feat_4[,.(ACCOUNT_ID, EVENT_ID, INPLAY_OUTPLAY_RATIO)])

    d <- data.table(d_raw)
    d <- d[,TRANSACTION_COUNT_TOTAL:=sum(TRANSACTION_COUNT, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')]
    feat_5 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, TRANSACTION_COUNT_TOTAL)])
    feat_5[,BACK_RATIO:=ifelse(is.na(TRANSACTION_COUNT),0,TRANSACTION_COUNT)/TRANSACTION_COUNT_TOTAL]
    feat_5 <- as.data.frame(feat_5)
    feat_5 <- feat_5[!duplicated(feat_5[,1:2]),]
feat_5 <- unique(data.table(feat_5)[,.(ACCOUNT_ID, EVENT_ID, BACK_RATIO)])

    d <- data.table(d_raw)
    d <- d[,BET_SIZE:=TRANSACTION_COUNT * AVG_BET_SIZE, by = c('ACCOUNT_ID', 'EVENT_ID')]
    d <- d[,TOTAL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')]
    feat_6 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, BET_SIZE, TOTAL_BET_SIZE)])
    feat_6[,BACK_RATIO_BET:=ifelse(is.na(BET_SIZE),0,BET_SIZE)/TOTAL_BET_SIZE]
    feat_6 <- as.data.frame(feat_6)
    feat_6 <- feat_6[!duplicated(feat_6[,1:2]),]
feat_6 <- unique(data.table(feat_6)[,.(ACCOUNT_ID, EVENT_ID, BACK_RATIO_BET)])


feat_7 <- unique(train[,.(ACCOUNT_ID, AVG_COUNTRY_RATIO, SD_COUNTRY_RATIO)])

# 5. Cancel rates
    dc <- data.table(dc)
    dc[,TRANSACTION_COUNT_CANCEL:=sum(TRANSACTION_COUNT, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,TOTAL_BET_SIZE_CANCEL:=sum(TRANSACTION_COUNT * AVG_BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,TRANSACTION_COUNT:=sum(TRANSACTION_COUNT, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')]
    feat_8 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, TOTAL_BET_SIZE)])
    feat_8_2 <- unique(dc[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT_CANCEL, TOTAL_BET_SIZE_CANCEL)])
    feat_8 <- merge(feat_8, feat_8_2, all.x = T, all.y = F, by =c('ACCOUNT_ID', 'EVENT_ID') )
feat_8 <- feat_8[,.(ACCOUNT_ID, EVENT_ID,
                    CANCEL_TRANS_RATIO = ifelse(is.na(TRANSACTION_COUNT_CANCEL/TRANSACTION_COUNT),0,TRANSACTION_COUNT_CANCEL/TRANSACTION_COUNT),
                    CANCEL_AMOUNTS_RATIO = ifelse(is.na(TOTAL_BET_SIZE_CANCEL/TOTAL_BET_SIZE),0,TOTAL_BET_SIZE_CANCEL/TOTAL_BET_SIZE))]

# 6. % of overall features (trans/amounts)
    d <- data.table(d_raw)
    d <- d[,OVERALL_BET_SIZE:=sum(TRANSACTION_COUNT * AVG_BET_SIZE, na.rm = T), by='EVENT_ID']
    d <- d[,OVERALL_BET_COUNT:=sum(TRANSACTION_COUNT, na.rm = T), by='EVENT_ID']
    d <- d[,TOTAL_BET_SIZE:=sum(TRANSACTION_COUNT * AVG_BET_SIZE, na.rm = T), by=c('ACCOUNT_ID', 'EVENT_ID')]
    d <- d[,TRANSACTION_COUNT:=sum(TRANSACTION_COUNT, na.rm = T), by=c('ACCOUNT_ID', 'EVENT_ID')]
    feat_9 <- unique(d[,.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE,TRANSACTION_COUNT,OVERALL_BET_SIZE,OVERALL_BET_COUNT)])
feat_9 <- feat_9[,.(ACCOUNT_ID, EVENT_ID, PNT_BET_SIZE =TOTAL_BET_SIZE/OVERALL_BET_SIZE*200, PNT_BET_COUNT = TRANSACTION_COUNT/OVERALL_BET_COUNT*200)]

# 7. lag win
    feat_10 <- unique(train[,.(ACCOUNT_ID, EVENT_ID, flag_class, PAST_LAG_1, PAST_LAG_2, PAST_LAG_3, PAST_LAG_4)])
    feat_10 <- feat_10[, LATEST_EVENT:=max(EVENT_ID), by='ACCOUNT_ID']
    feat_10 <- as.data.frame(feat_10)
    feat_10 <- feat_10[which(feat_10$EVENT_ID==feat_10$LATEST_EVENT),c(1,3:7)]
    names(feat_10) <- c('ACCOUNT_ID', 'PAST_LAG_1', 'PAST_LAG_2', 'PAST_LAG_3', 'PAST_LAG_4', 'PAST_LAG_5')
feat_10 <- unique(as.data.table(feat_10)[,.(ACCOUNT_ID, PAST_LAG_1,PAST_LAG_2,PAST_LAG_3,PAST_LAG_4,PAST_LAG_5)])

# Combine
test <- merge(feat_1, feat_2, all.x = T, all.y = F, by = c('ACCOUNT_ID'))
test <- merge(test, feat_3, all.x = T, all.y = F, by = c('ACCOUNT_ID'))
test <- merge(test, feat_4, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
test <- merge(test, feat_5, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
test <- merge(test, feat_6, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
test <- merge(test, feat_7, all.x = T, all.y = F, by = c('ACCOUNT_ID'))
test <- merge(test, feat_8, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
test <- merge(test, feat_9, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
test <- merge(test, feat_10, all.x = T, all.y = F, by = c('ACCOUNT_ID'))
test <- test[,flag_regr:= 0]
test <- test[,flag_class:= 0]

test
test <- as.data.frame(test)
test_new <- test[is.na(test$PAST_LAG_1),]
test <- test[!is.na(test$PAST_LAG_1),]
apply(test, 2, function(x) mean(is.na(x)))
apply(test_new, 2, function(x) mean(is.na(x)))
test[is.na(test)] <- 0
load('data/v3/train_20151115.RData')
save(train, test, test_new, file = 'data/v3/Ivan_train_test_20151115.RData')
