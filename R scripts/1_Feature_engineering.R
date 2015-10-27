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
    AVG_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, mean) 
    names(AVG_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_PLACED_TAKEN_TIME')

    # 7. STDEV_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    STDEV_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, sd) 
    names(STDEV_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_PLACED_TAKEN_TIME')
    STDEV_PLACED_TAKEN_TIME[is.na(STDEV_PLACED_TAKEN_TIME$STDEV_PLACED_TAKEN_TIME), 5] <- 0
    
    # 8. AVG_TAKEN_HOUR (?)
    AVG_TAKEN_HOUR <- aggregate(as.numeric(format(PLACED_DATE, "%H")) ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, mean) 
    names(AVG_TAKEN_HOUR) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_TAKEN_HOUR')
    
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
    NET_PROFIT <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, sum) 
    names(NET_PROFIT) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'NET_PROFIT')
    
    # 11. MARGIN (*)
    MARGIN <- aggregate(PRICE_TAKEN * BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, sum) 
    names(MARGIN) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MARGIN')
    MARGIN$MARGIN <- NET_PROFIT$NET_PROFIT/MARGIN$MARGIN
    
    # MERGE AND RETURN Member base
    submit <- merge(d, TRANSACTION_COUNT, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET'))
    
    
    # 10. CANCEL RATIO
    
    # 11. INPLAY RATIO
    
    
    #META DATA
    # 1. RFM
    
    # 2. CLUSTERING
    
    
}

d2 <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
