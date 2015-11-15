rm(list=ls());gc()
load('../Datathon_Full_Dataset/cleaned_raw_data.RData');d <- dt

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
#bet/taken diff
d$DIFF_BT <- (d$PRICE_TAKEN - d$BET_PRICE)/(d$BET_PRICE)

#####################
# start #############
#####################
# d <- rbind(d,dc)
library(data.table)
d <- data.table(d)
# 1. Bonus Feature
d[,TRANSACTION_COUNT:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID')] #
d[,AVG_BET_SIZE:=mean(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
d[,MAX_BET_SIZE:=max(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
d[,MIN_BET_SIZE:=min(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
d[,STDEV_BET_SIZE:=ifelse(is.na(sd(BET_SIZE, na.rm = T)), 0, sd(BET_SIZE, na.rm = T)), 
  by = c('ACCOUNT_ID', 'EVENT_ID')] #
d[,TOTAL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #

feat_1 <- d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, AVG_BET_SIZE, MAX_BET_SIZE, MIN_BET_SIZE, STDEV_BET_SIZE, TOTAL_BET_SIZE)]
feat_1 <- unique(feat_1)

# 2. Inplay Outplay ratio
feat_2 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TOTAL_BET_SIZE)])
d[,TOTAL_BET_SIZE_INOUT:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID', 'INPLAY_BET')]
feat_2_2 <- unique(d[INPLAY_BET=='Y',.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE_INOUT)])
feat_2 <- merge(feat_2,feat_2_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
feat_2[,INPLAY_OUTPLAY_RATIO:=ifelse(is.na(TOTAL_BET_SIZE_INOUT),0,TOTAL_BET_SIZE_INOUT)/TOTAL_BET_SIZE]
feat_2 <- unique(feat_2[,.(ACCOUNT_ID, EVENT_ID, INPLAY_OUTPLAY_RATIO)])

# 3. country 1 & country 2
d[,BET_COUNTRY_ONE_SIZE:=sum(ifelse(BET_COUNTRY_ONE == 1, BET_SIZE, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')] #2
d[,BET_COUNTRY_TWO_SIZE:=sum(ifelse(BET_COUNTRY_TWO == 1, BET_SIZE, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')] #2
d[,BET_COUNTRY_RATIO:=abs(BET_COUNTRY_ONE_SIZE-BET_COUNTRY_TWO_SIZE)/TOTAL_BET_SIZE]
feat_3 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, BET_COUNTRY_RATIO)])
feat_3_2 <- feat_3[,BET_COUNTRY_RATIO_CUM:=cumsum(BET_COUNTRY_RATIO)-BET_COUNTRY_RATIO, by = c('ACCOUNT_ID')]
feat_3_2 <- feat_3[,EVENT_COUNT:=1]
feat_3_2 <- feat_3[,EVENT_COUNT:=cumsum(EVENT_COUNT)-EVENT_COUNT, by = c('ACCOUNT_ID')]
feat_3_2 <- feat_3_2[,BET_COUNTRY_RATIO:=ifelse(EVENT_COUNT==0,0,BET_COUNTRY_RATIO/EVENT_COUNT), by = c('ACCOUNT_ID')]
feat_3 <- feat_3_2[,.(ACCOUNT_ID, EVENT_ID, BET_COUNTRY_RATIO)]

# 4. diff bet and taken


# 3. B & L
feat_4 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT)])
d <- d[,TRANSACTION_COUNT_BL:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID','BID_TYP')]
feat_4_2 <- unique(d[BID_TYP=='B',.(ACCOUNT_ID, EVENT_ID,TRANSACTION_COUNT_BL)])
feat_4 <- merge(feat_4,feat_4_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
feat_4[,BACK_RATIO:=ifelse(is.na(TRANSACTION_COUNT_BL),0,TRANSACTION_COUNT_BL)/TRANSACTION_COUNT]
feat_4 <- unique(feat_4[,.(ACCOUNT_ID, EVENT_ID, BACK_RATIO)])

feat_5 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TOTAL_BET_SIZE)])
d <- d[,TOTAL_BET_SIZE_BL:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID','BID_TYP')]
feat_5_2 <- unique(d[BID_TYP=='B',.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE_BL)])
feat_5 <- merge(feat_5,feat_5_2, all.x = T, all.y = F, by = c('ACCOUNT_ID', 'EVENT_ID'))
feat_5[,BACK_RATIO_BET:=ifelse(is.na(TOTAL_BET_SIZE_BL),0,TOTAL_BET_SIZE_BL)/TOTAL_BET_SIZE]
feat_5 <- unique(feat_5[,.(ACCOUNT_ID, EVENT_ID, BACK_RATIO_BET)])

# 4. Win hist & Margin
d <- d[,flag_regr:=sum(PROFIT_LOSS), by = c('ACCOUNT_ID', 'EVENT_ID')]
d <- d[,flag_class:=ifelse(flag_regr > 0, 1, ifelse(flag_regr < 0, -1, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')]
d <- d[,win_class:=ifelse(flag_regr > 0, 1, ifelse(flag_regr < 0, 0, 0)), by = c('ACCOUNT_ID', 'EVENT_ID')]
feat_6 <- unique(d[,.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE,flag_regr,flag_class,win_class)])
feat_6 <- feat_6[,MARGIN:=flag_regr/TOTAL_BET_SIZE, by = c('ACCOUNT_ID', 'EVENT_ID')]
feat_6 <- feat_6[,EVENT_NUM:=1]
feat_6 <- feat_6[,EXPERIENCE:=cumsum(EVENT_NUM)-EVENT_NUM, by = c('ACCOUNT_ID')]
feat_6 <- feat_6[,MARGIN_SO_FAR:=ifelse(EXPERIENCE == 0, 0, (cumsum(MARGIN)-MARGIN)/EXPERIENCE), by = c('ACCOUNT_ID')]
feat_6 <- feat_6[,WIN_HIST:=cumsum(flag_class)-flag_class, by = c('ACCOUNT_ID')]
feat_6_2 <- unique(feat_6[,.(EVENT_ID,1)])
feat_6_2 <- feat_6_2[,cnt:=cumsum(V2)]
feat_6 <- merge(feat_6, feat_6_2, all.x = T, all.y = F, by = 'EVENT_ID')
feat_6 <- feat_6[,FREQUENCY:=(EXPERIENCE+1)/cnt, by = c('ACCOUNT_ID')]
feat_6 <- feat_6[,WIN_RATIO:=ifelse(EXPERIENCE==0, 0.5, (cumsum(win_class)-win_class)/EXPERIENCE), by = c('ACCOUNT_ID')]
feat_6 <- feat_6[,.(ACCOUNT_ID, EVENT_ID, MARGIN_SO_FAR, EXPERIENCE, WIN_HIST, FREQUENCY, WIN_RATIO, flag_regr, flag_class)]

# 5. % of overall bet size, counts
d <- d[,OVERALL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by='EVENT_ID']
d <- d[,OVERALL_BET_COUNT:=sum(TRANSACTION_COUNT, na.rm = T), by='EVENT_ID']
feat_7 <- unique(d[,.(ACCOUNT_ID, EVENT_ID,TOTAL_BET_SIZE,TRANSACTION_COUNT,OVERALL_BET_SIZE,OVERALL_BET_COUNT)])
feat_7 <- feat_7[,.(ACCOUNT_ID, EVENT_ID, PNT_BET_SIZE =TOTAL_BET_SIZE/OVERALL_BET_SIZE*200, PNT_BET_COUNT = TRANSACTION_COUNT/OVERALL_BET_COUNT*200)]

# 6. Cancel ratio (transaction / amount)
feat_8 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, flag_class)])
feat_8 <- feat_8[,PAST_LAG_1 := shift(flag_class,0,-1), by = 'ACCOUNT_ID']
feat_8 <- feat_8[,PAST_LAG_2 := shift(flag_class,0,-2), by = 'ACCOUNT_ID']
feat_8 <- feat_8[,PAST_LAG_3 := shift(flag_class,0,-3), by = 'ACCOUNT_ID']
feat_8 <- feat_8[,PAST_LAG_4 := shift(flag_class,0,-4), by = 'ACCOUNT_ID']
feat_8 <- feat_8[,PAST_LAG_5 := shift(flag_class,0,-5), by = 'ACCOUNT_ID']
feat_8 <- unique(feat_8[,.(ACCOUNT_ID, EVENT_ID, PAST_LAG_1,PAST_LAG_2,PAST_LAG_3,PAST_LAG_4,PAST_LAG_5)])

# Inplay ratio
# Country 1 & Country 2
# diff bet and taken
# B & L Diff
# Win hist
# Margin
# Frequency
# experience
# % of overall of event
# lag 

# 7. Cancel ratio (transaction / amount)
dc <- data.table(dc)
dc[,TRANSACTION_COUNT_CANCEL:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID')] #
dc[,TOTAL_BET_SIZE_CANCEL:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
feat_9 <- unique(d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, TOTAL_BET_SIZE)])
feat_9_2 <- unique(dc[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT_CANCEL, TOTAL_BET_SIZE_CANCEL)])
feat_9 <- merge(feat_9, feat_9_2, all.x = T, all.y = F, by =c('ACCOUNT_ID', 'EVENT_ID') )
feat_9 <- feat_9[,.(ACCOUNT_ID, EVENT_ID, 
                    CANCEL_TRANS_RATIO = ifelse(is.na(TRANSACTION_COUNT_CANCEL/TRANSACTION_COUNT),0,TRANSACTION_COUNT_CANCEL/TRANSACTION_COUNT),
                    CANCEL_AMOUNTS_RATIO = ifelse(is.na(TOTAL_BET_SIZE_CANCEL/TOTAL_BET_SIZE),0,TOTAL_BET_SIZE_CANCEL/TOTAL_BET_SIZE))]

train <- merge(feat_1, feat_2, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_3, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_4, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_5, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_6, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_7, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_8, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))
train <- merge(train, feat_9, all = T, by = c('ACCOUNT_ID', 'EVENT_ID'))

train
train <- as.data.frame(train)
apply(train, 2, function(x) mean(is.na(x)))
train <- train[,c(1:17,20:28,18:19)] 
save(train, file = 'train_20151115.RData')
load('train_20151115.RData')
