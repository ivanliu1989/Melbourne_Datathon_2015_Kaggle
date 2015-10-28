setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

dt_1 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 1-10.csv', stringsAsFactors=FALSE,na.strings = "")
dt_2 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 11-20.csv', stringsAsFactors=FALSE,na.strings = "")
dt_3 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 21-30.csv', stringsAsFactors=FALSE,na.strings = "")
dt_4 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 31-40.csv', stringsAsFactors=FALSE,na.strings = "")
dt_5 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games QTR Finals.csv', stringsAsFactors=FALSE,na.strings = "")

dt <- rbind(dt_1, dt_2, dt_3, dt_4, dt_5)

feat.eng <- function(d){
    #trim the white space
    d$STATUS_ID <- trimws(d$STATUS_ID)
    d$BID_TYP <- trimws(d$BID_TYP)
    
    #the columns of interest 
    requiredColumns <- c('ACCOUNT_ID','EVENT_ID','BID_TYP','PRICE_TAKEN','BET_SIZE','STATUS_ID','PROFIT_LOSS',
                         'INPLAY_BET','PLACED_DATE','TAKEN_DATE','MATCH')
    # INPLAY_BET, PLACED_DATE-TAKEN_DATE, TAKEN_TIME, COUNTRY, TEAM_WIN_RATES, CUSTOMER_TYPE, PREV_WIN, PREV_AMOUNT
    # Average bet size / current bet size
    
    #rows of interest - just the settled bets
    requiredRows <- which(d$STATUS_ID == 'S')
    requiredRows2 <- which(d$STATUS_ID == 'L')
    requiredRows3 <- which(d$STATUS_ID == 'C')
    
    #filter
    dl <- d[requiredRows2,requiredColumns]
    dc <- d[requiredRows3,requiredColumns]
    d <- d[requiredRows,requiredColumns]
    
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
    
    d$PLACED_DATE <- strptime(d$PLACED_DATE, "%d/%m/%Y %I:%M:%S %p")
    d$TAKEN_DATE <- strptime(d$TAKEN_DATE, "%d/%m/%Y %I:%M:%S %p")
    d$PLACED_TAKEN_TIME <- as.numeric(d$TAKEN_DATE - d$PLACED_DATE)
    
    #NEW FEATURES
    # 1. TRANSACTION_COUNT
    TRANSACTION_COUNT <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, length) 
    names(TRANSACTION_COUNT) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT')
    
    TRANSACTION_COUNT_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, length) 
    names(TRANSACTION_COUNT_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_L')
    
    TRANSACTION_COUNT_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, length) 
    names(TRANSACTION_COUNT_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_C')
    
    # 2. AVG_BET_SIZE
    AVG_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, mean) 
    names(AVG_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE')
    
    AVG_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, mean) 
    names(AVG_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_L')
    
    AVG_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, mean) 
    names(AVG_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_C')
    
    # 3. MAX_BET_SIZE
    MAX_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, max) 
    names(MAX_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE')
    
    MAX_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, max) 
    names(MAX_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_L')
    
    MAX_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, max) 
    names(MAX_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_C')
    
    # 4. MIN_BET_SIZE
    MIN_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, min) 
    names(MIN_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE')
    
    MIN_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, min) 
    names(MIN_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_L')
    
    MIN_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, min) 
    names(MIN_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_C')
    
    # 5. STDEV_BET_SIZE
    STDEV_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, sd) 
    names(STDEV_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE')
    STDEV_BET_SIZE[is.na(STDEV_BET_SIZE$STDEV_BET_SIZE), 5] <- 0
    
    STDEV_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, sd) 
    names(STDEV_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_L')
    STDEV_BET_SIZE_L[is.na(STDEV_BET_SIZE_L$STDEV_BET_SIZE_L), 5] <- 0
    
    STDEV_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, sd) 
    names(STDEV_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_C')
    STDEV_BET_SIZE_C[is.na(STDEV_BET_SIZE_C$STDEV_BET_SIZE_C), 5] <- 0
    
    # 6. AVG_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    AVG_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, mean) #EVENT_ID
    names(AVG_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_PLACED_TAKEN_TIME')

    # 7. STDEV_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    STDEV_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sd) 
    names(STDEV_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_PLACED_TAKEN_TIME') #EVENT_ID
    STDEV_PLACED_TAKEN_TIME[is.na(STDEV_PLACED_TAKEN_TIME$STDEV_PLACED_TAKEN_TIME), 5] <- 0
    
    # 8. AVG_TAKEN_HOUR (?)
    AVG_TAKEN_HOUR <- aggregate(as.numeric(format(PLACED_DATE, "%H")) ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, mean) #EVENT_ID
    names(AVG_TAKEN_HOUR) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_TAKEN_HOUR')
    
    # 9. PREV_WIN_RATE (*)
    PREV_WIN_RATE_W <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d[d$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, length) 
    
    PREV_WIN_RATE_W_A <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d[d$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A_A <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d, length) 
    
    REV_WIN_RATE <- merge(PREV_WIN_RATE_W, PREV_WIN_RATE_A, all = T, by = c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET'))
    names(REV_WIN_RATE) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'WIN', 'ALL')
    REV_WIN_RATE[is.na(REV_WIN_RATE$WIN), 'WIN'] <- 0
    REV_WIN_RATE$REV_WIN_RATE <- REV_WIN_RATE$WIN/REV_WIN_RATE$ALL
    REV_WIN_RATE$WIN <- NULL; REV_WIN_RATE$ALL <- NULL
    
    REV_WIN_RATE_ALL <- merge(PREV_WIN_RATE_W_A, PREV_WIN_RATE_A_A, all = T, by = c('ACCOUNT_ID', 'STATUS_ID'))
    names(REV_WIN_RATE_ALL) <- c('ACCOUNT_ID', 'STATUS_ID', 'WIN', 'ALL')
    REV_WIN_RATE_ALL[is.na(REV_WIN_RATE_ALL$WIN), 'WIN'] <- 0
    REV_WIN_RATE_ALL$REV_WIN_RATE_ALL <- REV_WIN_RATE_ALL$WIN/REV_WIN_RATE_ALL$ALL
    REV_WIN_RATE_ALL$WIN <- NULL; REV_WIN_RATE_ALL$ALL <- NULL
    
    PREV_WIN_RATE <- merge(REV_WIN_RATE, REV_WIN_RATE_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'STATUS_ID'))
    
    # 10. NET_PROFIT (*)
    NET_PROFIT <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sum)  #EVENT_ID
    names(NET_PROFIT) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'NET_PROFIT')
    
    # 11. MARGIN (*)
    MARGIN <- aggregate(PRICE_TAKEN * BET_SIZE ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sum) 
    names(MARGIN) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'MARGIN') #EVENT_ID
    MARGIN$MARGIN <- NET_PROFIT$NET_PROFIT/MARGIN$MARGIN
    
    # MERGE AND RETURN Member base
    mbr.event <- dt[!duplicated(paste0(dt$ACCOUNT_ID, dt$EVENT_ID)),c('ACCOUNT_ID', 'EVENT_ID')]
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT[TRANSACTION_COUNT$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT[TRANSACTION_COUNT$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L[TRANSACTION_COUNT_L$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L[TRANSACTION_COUNT_L$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C[TRANSACTION_COUNT_C$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C[TRANSACTION_COUNT_C$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_BET_SIZE[AVG_BET_SIZE$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE[AVG_BET_SIZE$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L[AVG_BET_SIZE_L$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L[AVG_BET_SIZE_L$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C[AVG_BET_SIZE_C$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C[AVG_BET_SIZE_C$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, MAX_BET_SIZE[MAX_BET_SIZE$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE[MAX_BET_SIZE$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L[MAX_BET_SIZE_L$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L[MAX_BET_SIZE_L$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C[MAX_BET_SIZE_C$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C[MAX_BET_SIZE_C$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, MIN_BET_SIZE[MIN_BET_SIZE$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE[MIN_BET_SIZE$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L[MIN_BET_SIZE_L$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L[MIN_BET_SIZE_L$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C[MIN_BET_SIZE_C$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C[MIN_BET_SIZE_C$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE[STDEV_BET_SIZE$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE[STDEV_BET_SIZE$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L[STDEV_BET_SIZE_L$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L[STDEV_BET_SIZE_L$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C[STDEV_BET_SIZE_C$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C[STDEV_BET_SIZE_C$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME[AVG_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME[AVG_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME[STDEV_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME[STDEV_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR[AVG_TAKEN_HOUR$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR[AVG_TAKEN_HOUR$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, PREV_WIN_RATE[PREV_WIN_RATE$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, PREV_WIN_RATE[PREV_WIN_RATE$INPLAY_BET == 'N', c(1,4,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, NET_PROFIT[NET_PROFIT$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, NET_PROFIT[NET_PROFIT$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, MARGIN[MARGIN$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, MARGIN[MARGIN$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    names(mbr.event) <- c('ACCOUNT_ID','EVENT_ID','TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_INPLAY_L','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_INPLAY_C','TRANSACTION_COUNT_OUTPLAY_C',
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
    mbr.event[, -c(1,2,33:41)][is.na(mbr.event[, -c(1,2,33:41)])] <- 0
    mbr.event[, c(33:36)][is.na(mbr.event[, c(33:36)])] <- 0
    mbr.event[, c(39:41)][is.na(mbr.event[, c(39:41)])] <- 0.5
    mbr.event$AVG_TAKEN_HOUR_INPLAY <- floor(mbr.event$AVG_TAKEN_HOUR_INPLAY)
    mbr.event$AVG_TAKEN_HOUR_OUTPLAY <- floor(mbr.event$AVG_TAKEN_HOUR_OUTPLAY)
    
    # 10. CANCEL_RATIO
    mbr.event$CANCEL_RATIO_INPLAY <- mbr.event$TRANSACTION_COUNT_INPLAY_C/(mbr.event$TRANSACTION_COUNT_INPLAY_C+mbr.event$TRANSACTION_COUNT_INPLAY+mbr.event$TRANSACTION_COUNT_INPLAY_L)
    mbr.event$CANCEL_RATIO_OUTPLAY <- mbr.event$TRANSACTION_COUNT_OUTPLAY_C/(mbr.event$TRANSACTION_COUNT_OUTPLAY_C+mbr.event$TRANSACTION_COUNT_OUTPLAY+mbr.event$TRANSACTION_COUNT_OUTPLAY_L)
        
    # 11. INPLAY_RATIO
    mbr.event$INPLAY_RATIO <- mbr.event$TRANSACTION_COUNT_INPLAY/(mbr.event$TRANSACTION_COUNT_INPLAY + mbr.event$TRANSACTION_COUNT_OUTPLAY)
    mbr.event[,c(46:48)][is.na(mbr.event[,c(46:48)])] <- 0
    
    #META DATA
    # 1. RFM
    
    # 2. CLUSTERING
    
    # Result
    flag <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID, data=d, sum, na.rm = T) 
    flag$flag_class <- ifelse(flag$PROFIT_LOSS<0, 'N', 'Y')
    names(flag) <- c('ACCOUNT_ID','EVENT_ID','flag_regr','flag_class')
    
    mbr.event <- merge(mbr.event, flag, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    return(mbr.event)
}

mbr.event <- feat.eng(dt)

save(mbr.event, file='data/mbr_event_data.RData')

d2 <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
