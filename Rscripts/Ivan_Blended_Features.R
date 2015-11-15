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
    d[,STDEV_BET_SIZE:=ifelse(is.na(sd(BET_SIZE, na.rm = T)), 0, sd(BET_SIZE, na.rm = T)), 
      by = c('ACCOUNT_ID', 'EVENT_ID')] #
    d[,TOTAL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_1 <- d[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, AVG_BET_SIZE, MAX_BET_SIZE, MIN_BET_SIZE, STDEV_BET_SIZE, TOTAL_BET_SIZE)]
    feat_1 <- unique(feat_1)
    
    dc[,TRANSACTION_COUNT:=length(BET_SIZE), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,AVG_BET_SIZE:=mean(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,MAX_BET_SIZE:=max(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,MIN_BET_SIZE:=min(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,STDEV_BET_SIZE:=ifelse(is.na(sd(BET_SIZE, na.rm = T)), 0, sd(BET_SIZE, na.rm = T)), 
      by = c('ACCOUNT_ID', 'EVENT_ID')] #
    dc[,TOTAL_BET_SIZE:=sum(BET_SIZE, na.rm = T), by = c('ACCOUNT_ID', 'EVENT_ID')] #
    feat_1_c <- dc[,.(ACCOUNT_ID, EVENT_ID, TRANSACTION_COUNT, AVG_BET_SIZE, MAX_BET_SIZE, MIN_BET_SIZE, STDEV_BET_SIZE, TOTAL_BET_SIZE)]
    feat_1_c <- unique(feat_1_c)
    
# 2. Taken time / placed hour
    
# 3. Win hist / profit / margin
    
# 4. Inplay/Outplay | Bet/lay rate | Country ratio
    
# 5. Cancel rates
    
# 6. % of overall features (trans/amounts)
    
# 7. Experience / frequency / lag win

