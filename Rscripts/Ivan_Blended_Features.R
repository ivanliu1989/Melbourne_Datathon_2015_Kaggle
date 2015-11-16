rm(list=ls());gc()
load('../Datathon_Full_Dataset/cleaned_raw_data.RData');d <- dt
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
    #trim the white space
    d$STATUS_ID <- trimws(d$STATUS_ID)
    d$BID_TYP <- trimws(d$BID_TYP)
    #rows of interest - just the settled bets
    requiredRows <- which(d$STATUS_ID == 'S')
    requiredRows2 <- which(d$STATUS_ID == 'C')
    #filter
    dc <- d[requiredRows2,]
    d <- d[requiredRows,]
    #correct the profit_loss calculation
    d$PROFIT_LOSS1 <- 0.0
    r1 <- d$PROFIT_LOSS > 0 & d$BID_TYP == 'B'
    r2 <- d$PROFIT_LOSS > 0 & d$BID_TYP == 'L'
    r3 <- d$PROFIT_LOSS < 0 & d$BID_TYP == 'L'
    r4 <- d$PROFIT_LOSS < 0 & d$BID_TYP == 'B'
    d$PROFIT_LOSS1[r1] <- (d$PRICE_TAKEN[r1] - 1.0)  * d$BET_SIZE[r1]
    d$PROFIT_LOSS1[r2] <- d$BET_SIZE[r2]
    d$PROFIT_LOSS1[r3] <- (d$PRICE_TAKEN[r3] - 1.0)  * -1.0 * d$BET_SIZE[r3]
    d$PROFIT_LOSS1[r4] <- -1.0 * d$BET_SIZE[r4]
    d$PROFIT_LOSS <- d$PROFIT_LOSS1
    d$PROFIT_LOSS1 <- NULL
    #date format
    d$PLACED_DATE <- strptime(d$PLACED_DATE, "%d/%m/%Y %I:%M:%S %p")
    d$TAKEN_DATE <- strptime(d$TAKEN_DATE, "%d/%m/%Y %I:%M:%S %p")
    d$OFF_DT <- strptime(d$OFF_DT, "%d/%m/%Y %I:%M:%S %p")
    d$PLACED_TAKEN_TIME <- as.numeric(d$TAKEN_DATE - d$PLACED_DATE)
    d$PLACED_HOUR <- as.numeric(format(d$PLACED_DATE, "%H"))
    d$PLACED_DATE <- NULL
    d$TAKEN_DATE <- NULL
    d$OFF_DT <- NULL
    #bet/taken diff
    d$DIFF_BT <- (d$PRICE_TAKEN - d$BET_PRICE)/(d$BET_PRICE)
    d$BET_COUNTRY_SIZE <- ifelse(d$BET_COUNTRY == 1, d$BET_SIZE, 0)
    d$BET_OP_COUNTRY_SIZE <- ifelse(d$BET_OP_COUNTRY == 1, d$BET_SIZE, 0)
    d$BET_COUNTRY_ONE_SIZE <- ifelse(d$BET_COUNTRY_ONE == 1, d$BET_SIZE, 0)
    d$BET_COUNTRY_TWO_SIZE <- ifelse(d$BET_COUNTRY_TWO == 1, d$BET_SIZE, 0)
    
#############################    
# Feature engineering #######
#############################
library(data.table)
d <- data.table(d)
dc <- data.table(dc)
# 1. Count/Bet features
    d[,TRANSACTION_COUNT:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,AVG_BET_SIZE:=mean(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,MAX_BET_SIZE:=max(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,MIN_BET_SIZE:=min(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,STDEV_BET_SIZE:=ifelse(is.na(sd(BET_SIZE, na.rm = T)), 0, sd(BET_SIZE, na.rm = T)),by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,TOTAL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_1 <- d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, AVG_BET_SIZE, MAX_BET_SIZE, MIN_BET_SIZE, STDEV_BET_SIZE, TOTAL_BET_SIZE)]
    feat_1 <- unique(feat_1)
    
    dc[,TRANSACTION_COUNT_C:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,AVG_BET_SIZE_C:=mean(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,MAX_BET_SIZE_C:=max(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,MIN_BET_SIZE_C:=min(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,STDEV_BET_SIZE_C:=ifelse(is.na(sd(BET_SIZE, na.rm = T)), 0, sd(BET_SIZE, na.rm = T)),by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,TOTAL_BET_SIZE_C:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_1_c <- dc[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT_C, AVG_BET_SIZE_C, MAX_BET_SIZE_C, MIN_BET_SIZE_C, STDEV_BET_SIZE_C, TOTAL_BET_SIZE_C)]
    feat_1_c <- unique(feat_1_c)
    
feat_1 <- merge(feat_1, feat_1_c, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))

# 2. Taken time / placed hour
    d[,SD_PLACED_TAKEN_TIME:=sd(PLACED_TAKEN_TIME, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,PERC_PLACED_TAKEN_TIME_1:=quantile(PLACED_TAKEN_TIME, probs = 0, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,PERC_PLACED_TAKEN_TIME_2:=quantile(PLACED_TAKEN_TIME, probs = 0.25, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,PERC_PLACED_TAKEN_TIME_3:=quantile(PLACED_TAKEN_TIME, probs = 0.5, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,PERC_PLACED_TAKEN_TIME_4:=quantile(PLACED_TAKEN_TIME, probs = 0.75, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,PERC_PLACED_TAKEN_TIME_5:=quantile(PLACED_TAKEN_TIME, probs = 1, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,SD_PLACED_HOUR:=sd(PLACED_HOUR, na.rm=T), by = c('ACCOUNT_ID')] 
    d[,AVG_PLACED_HOUR:=mean(PLACED_HOUR, na.rm=T), by = c('ACCOUNT_ID')] 
    feat_2 <- d[,.(ACCOUNT_ID, EVENT_ID, SD_PLACED_TAKEN_TIME, PERC_PLACED_TAKEN_TIME_1, PERC_PLACED_TAKEN_TIME_2, PERC_PLACED_TAKEN_TIME_3, PERC_PLACED_TAKEN_TIME_4, PERC_PLACED_TAKEN_TIME_5,
                   SD_PLACED_HOUR, AVG_PLACED_HOUR)]
feat_2 <- unique(feat_2)

# 3. Win hist / profit / margin / Experience / frequency
    d[,EXPERIENCE:=1, by = c('ACCOUNT_ID','EVENT_ID')] 
    d[,TRANSACTION_WIN_LOSS:=ifelse(PROFIT_LOSS>0, 1, 0)] 
    d[,EVENT_PROFIT:=sum(PROFIT_LOSS, na.rm = T), by = c('ACCOUNT_ID','EVENT_ID')] 
    d[,EVENT_INVEST:=sum(BET_SIZE * PRICE_TAKEN, na.rm = T), by = c('ACCOUNT_ID','EVENT_ID')] 
    d[,EVENT_WIN_LOSS:=ifelse(EVENT_PROFIT>0, 1, ifelse(EVENT_PROFIT<0, -1, 0))] 
    
    feat_3_2 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, EXPERIENCE, EVENT_PROFIT, EVENT_INVEST, EVENT_WIN_LOSS)])
    feat_3_2[,EXPERIENCE:=sum(EXPERIENCE, na.rm = T), by=c('ACCOUNT_ID')]
    feat_3_2[,WIN_HIST:=sum(EVENT_WIN_LOSS, na.rm = T), by=c('ACCOUNT_ID')]
    feat_3_2[,MARGIN:=sum(EVENT_PROFIT, na.rm = T)/sum(EVENT_INVEST, na.rm = T), by=c('ACCOUNT_ID')]
    feat_3_2[,MARGIN_EVENT:=sum(EVENT_PROFIT, na.rm = T)/sum(EVENT_INVEST, na.rm = T), by=c('ACCOUNT_ID', 'EVENT_ID')]
    feat_3_2[,SD_MARGIN_EVENT:=sd(MARGIN_EVENT, na.rm = T), by=c('ACCOUNT_ID')]
    feat_3_2[,AVG_MARGIN_EVENT:=mean(MARGIN_EVENT, na.rm = T), by=c('ACCOUNT_ID')]
    feat_3_2[,WINS:=ifelse(EVENT_WIN_LOSS>0, 1, 0)]
    feat_3_2[,WIN_RATE:=sum(WINS)/EXPERIENCE, by=c('ACCOUNT_ID')]
    feat_3_2[,FREQUENCY:=EXPERIENCE/43, by=c('ACCOUNT_ID')]

feat_3 <- unique(feat_3_2[,.(ACCOUNT_ID, EVENT_ID, EXPERIENCE, WIN_HIST, MARGIN, SD_MARGIN_EVENT, AVG_MARGIN_EVENT, WIN_RATE, FREQUENCY)])

# 4. Inplay/Outplay | Bet/lay rate | Country ratio
    feat_4 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TOTAL_BET_SIZE)])
    d[,TOTAL_BET_SIZE_INOUT:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID', 'INPLAY_BET')]
    feat_4_2 <- unique(d[INPLAY_BET=='Y',.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE_INOUT)])
    feat_4 <- merge(feat_4,feat_4_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
    feat_4[,INPLAY_OUTPLAY_RATIO:=ifelse(is.na(TOTAL_BET_SIZE_INOUT),0,TOTAL_BET_SIZE_INOUT)/TOTAL_BET_SIZE]
feat_4 <- unique(feat_4[,.(ACCOUNT_ID, EVENT_ID, INPLAY_OUTPLAY_RATIO)])

    feat_5 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT)])
    d <- d[,TRANSACTION_COUNT_BL:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID','BID_TYP')]
    feat_5_2 <- unique(d[BID_TYP=='B',.(ACCOUNT_ID, EVENT_ID,TRANSACTION_COUNT_BL)])
    feat_5 <- merge(feat_5,feat_5_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
    feat_5[,BACK_RATIO:=ifelse(is.na(TRANSACTION_COUNT_BL),0,TRANSACTION_COUNT_BL)/TRANSACTION_COUNT]
feat_5 <- unique(feat_5[,.(ACCOUNT_ID, EVENT_ID, BACK_RATIO)])
    
    feat_6 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TOTAL_BET_SIZE)])
    d <- d[,TOTAL_BET_SIZE_BL:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID','BID_TYP')]
    feat_6_2 <- unique(d[BID_TYP=='B',.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE_BL)])
    feat_6 <- merge(feat_6,feat_6_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
    feat_6[,BACK_RATIO_BET:=ifelse(is.na(TOTAL_BET_SIZE_BL),0,TOTAL_BET_SIZE_BL)/TOTAL_BET_SIZE]
feat_6 <- unique(feat_6[,.(ACCOUNT_ID, EVENT_ID, BACK_RATIO_BET)])
    
    d[,BET_COUNTRY_ONE_SIZE:=sum(ifelse(BET_COUNTRY_ONE == 1, BET_SIZE, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')] #2
    d[,BET_COUNTRY_TWO_SIZE:=sum(ifelse(BET_COUNTRY_TWO == 1, BET_SIZE, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')] #2
    d[,BET_COUNTRY_RATIO:=abs(BET_COUNTRY_ONE_SIZE-BET_COUNTRY_TWO_SIZE)/TOTAL_BET_SIZE]
    feat_7 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, BET_COUNTRY_RATIO)])
    feat_7 <- feat_7[,AVG_COUNTRY_RATIO:=mean(BET_COUNTRY_RATIO, na.rm=T), by=c('ACCOUNT_ID')]
    feat_7 <- feat_7[,SD_COUNTRY_RATIO:=sd(BET_COUNTRY_RATIO, na.rm=T), by=c('ACCOUNT_ID')]
    
feat_7 <- unique(feat_7[,.(ACCOUNT_ID, EVENT_ID, AVG_COUNTRY_RATIO, SD_COUNTRY_RATIO)])

# 5. Cancel rates
    dc <- data.table(dc)
    dc[,TRANSACTION_COUNT_CANCEL:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,TOTAL_BET_SIZE_CANCEL:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_8 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, TOTAL_BET_SIZE)])
    feat_8_2 <- unique(dc[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT_CANCEL, TOTAL_BET_SIZE_CANCEL)])
    feat_8 <- merge(feat_8, feat_8_2, all.x = T, all.y = F, by =c('ACCOUNT_ID', 'EVENT_ID') )
feat_8 <- feat_8[,.(ACCOUNT_ID, EVENT_ID,
                    CANCEL_TRANS_RATIO = ifelse(is.na(TRANSACTION_COUNT_CANCEL/TRANSACTION_COUNT),0,TRANSACTION_COUNT_CANCEL/TRANSACTION_COUNT),
                    CANCEL_AMOUNTS_RATIO = ifelse(is.na(TOTAL_BET_SIZE_CANCEL/TOTAL_BET_SIZE),0,TOTAL_BET_SIZE_CANCEL/TOTAL_BET_SIZE))]

# 6. % of overall features (trans/amounts)
    d <- d[,OVERALL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by='EVENT_ID']
    d <- d[,OVERALL_BET_COUNT:=sum(TRANSACTION_COUNT, na.rm = T), by='EVENT_ID']
    feat_9 <- unique(d[,.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE,TRANSACTION_COUNT,OVERALL_BET_SIZE,OVERALL_BET_COUNT)])
feat_9 <- feat_9[,.(ACCOUNT_ID, EVENT_ID, PNT_BET_SIZE =TOTAL_BET_SIZE/OVERALL_BET_SIZE*200, PNT_BET_COUNT = TRANSACTION_COUNT/OVERALL_BET_COUNT*200)]

# 7. lag win
    d <- d[,flag_regr:=sum(PROFIT_LOSS), by = c('ACCOUNT_ID', 'EVENT_ID')]
    d <- d[,flag_class:=ifelse(flag_regr > 0, 1, ifelse(flag_regr < 0, -1, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')]
    feat_10 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, flag_class)])
    feat_10 <- feat_10[,PAST_LAG_1 := shift(flag_class,0,-1), by = 'ACCOUNT_ID']
    feat_10 <- feat_10[,PAST_LAG_2 := shift(flag_class,0,-2), by = 'ACCOUNT_ID']
    feat_10 <- feat_10[,PAST_LAG_3 := shift(flag_class,0,-3), by = 'ACCOUNT_ID']
    feat_10 <- feat_10[,PAST_LAG_4 := shift(flag_class,0,-4), by = 'ACCOUNT_ID']
    feat_10 <- feat_10[,PAST_LAG_5 := shift(flag_class,0,-5), by = 'ACCOUNT_ID']
feat_10 <- unique(feat_10[,.(ACCOUNT_ID, EVENT_ID, PAST_LAG_1,PAST_LAG_2,PAST_LAG_3,PAST_LAG_4,PAST_LAG_5)])

# Combine
target <- unique(d[,.(ACCOUNT_ID, EVENT_ID, flag_regr, flag_class)])
train <- merge(feat_1, feat_2, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_3, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_4, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_5, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_6, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_7, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_8, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_9, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_10, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, target, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))

train
train <- as.data.frame(train)
apply(train, 2, function(x) mean(is.na(x)))
train[is.na(train)] <- 0
save(train, file = 'train_20151115.RData')
load('train_20151115.RData')