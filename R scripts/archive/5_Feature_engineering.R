setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

dt_1 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 1-10.csv', stringsAsFactors=FALSE,na.strings = "")
dt_2 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 11-20.csv', stringsAsFactors=FALSE,na.strings = "")
dt_3 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 21-30.csv', stringsAsFactors=FALSE,na.strings = "")
dt_4 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games 31-40.csv', stringsAsFactors=FALSE,na.strings = "")
dt_5 <- read.csv('../Datathon_Full_Dataset/Datathon WC Data Games QTR Finals.csv', stringsAsFactors=FALSE,na.strings = "")

dt <- rbind(dt_1, dt_2, dt_3, dt_4, dt_5)
rm(list = c('dt_1', 'dt_2', 'dt_3', 'dt_4', 'dt_5'))

feat.eng <- function(d){
    #trim the white space
    d$STATUS_ID <- trimws(d$STATUS_ID)
    d$BID_TYP <- trimws(d$BID_TYP)
    
    #the columns of interest 
    requiredColumns <- c('ACCOUNT_ID','EVENT_ID','BID_TYP','PRICE_TAKEN','BET_SIZE','STATUS_ID','PROFIT_LOSS',
                         'INPLAY_BET','PLACED_DATE','TAKEN_DATE','MATCH','COUNTRY_OF_RESIDENCE_NAME')
    # INPLAY_BET, PLACED_DATE-TAKEN_DATE, TAKEN_TIME, COUNTRY, TEAM_WIN_RATES, CUSTOMER_TYPE, PREV_WIN, PREV_AMOUNT
    # Average bet size / current bet size
    
    #rows of interest - just the settled bets
    requiredRows <- which(d$STATUS_ID == 'S' & d$BID_TYP == 'B')
    requiredRows2 <- which(d$STATUS_ID == 'L' & d$BID_TYP == 'B')
    requiredRows3 <- which(d$STATUS_ID == 'C' & d$BID_TYP == 'B')
    
    requiredRows4 <- which(d$STATUS_ID == 'S' & d$BID_TYP == 'L')
    requiredRows5 <- which(d$STATUS_ID == 'L' & d$BID_TYP == 'L')
    requiredRows6 <- which(d$STATUS_ID == 'C' & d$BID_TYP == 'L')
    
    #filter
    dl_b <- d[requiredRows2,requiredColumns]
    dc_b <- d[requiredRows3,requiredColumns]
    d_b <- d[requiredRows,requiredColumns]
    
    dl_l <- d[requiredRows5,requiredColumns]
    dc_l <- d[requiredRows6,requiredColumns]
    d_l <- d[requiredRows4,requiredColumns]
    
    #correct the profit_loss calculation
    d_b$PROFIT_LOSS1 <- 0.0
    r1 <- d_b$PROFIT_LOSS > 0 & d_b$BID_TYP == 'B'
    r2 <- d_b$PROFIT_LOSS > 0 & d_b$BID_TYP == 'L'
    r3 <- d_b$PROFIT_LOSS < 0 & d_b$BID_TYP == 'L'
    r4 <- d_b$PROFIT_LOSS < 0 & d_b$BID_TYP == 'B'
    
    d_b$PROFIT_LOSS1[r1] <- (d_b$PRICE_TAKEN[r1] - 1.0)  * d_b$BET_SIZE[r1]
    d_b$PROFIT_LOSS1[r2] <- d_b$BET_SIZE[r2]
    d_b$PROFIT_LOSS1[r3] <- (d_b$PRICE_TAKEN[r3] - 1.0)  * -1.0 * d_b$BET_SIZE[r3]
    d_b$PROFIT_LOSS1[r4] <- -1.0 * d_b$BET_SIZE[r4]
    d_b$PROFIT_LOSS <- d_b$PROFIT_LOSS1
    d_b$PROFIT_LOSS1 <- NULL
    
    d_b$PLACED_DATE <- strptime(d_b$PLACED_DATE, "%d/%m/%Y %I:%M:%S %p")
    d_b$TAKEN_DATE <- strptime(d_b$TAKEN_DATE, "%d/%m/%Y %I:%M:%S %p")
    d_b$PLACED_TAKEN_TIME <- as.numeric(d_b$TAKEN_DATE - d_b$PLACED_DATE)
    
    d_l$PROFIT_LOSS1 <- 0.0
    r1 <- d_l$PROFIT_LOSS > 0 & d_l$BID_TYP == 'B'
    r2 <- d_l$PROFIT_LOSS > 0 & d_l$BID_TYP == 'L'
    r3 <- d_l$PROFIT_LOSS < 0 & d_l$BID_TYP == 'L'
    r4 <- d_l$PROFIT_LOSS < 0 & d_l$BID_TYP == 'B'
    
    d_l$PROFIT_LOSS1[r1] <- (d_l$PRICE_TAKEN[r1] - 1.0)  * d_l$BET_SIZE[r1]
    d_l$PROFIT_LOSS1[r2] <- d_l$BET_SIZE[r2]
    d_l$PROFIT_LOSS1[r3] <- (d_l$PRICE_TAKEN[r3] - 1.0)  * -1.0 * d_l$BET_SIZE[r3]
    d_l$PROFIT_LOSS1[r4] <- -1.0 * d_l$BET_SIZE[r4]
    d_l$PROFIT_LOSS <- d_l$PROFIT_LOSS1
    d_l$PROFIT_LOSS1 <- NULL
    
    d_l$PLACED_DATE <- strptime(d_l$PLACED_DATE, "%d/%m/%Y %I:%M:%S %p")
    d_l$TAKEN_DATE <- strptime(d_l$TAKEN_DATE, "%d/%m/%Y %I:%M:%S %p")
    d_l$PLACED_TAKEN_TIME <- as.numeric(d_l$TAKEN_DATE - d_l$PLACED_DATE)
    
    #NEW FEATURES
    # 1. TRANSACTION_COUNT
    TRANSACTION_COUNT_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_b, length) 
    names(TRANSACTION_COUNT_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_b')
    
    TRANSACTION_COUNT_L_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_b, length) 
    names(TRANSACTION_COUNT_L_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_L_b')
    
    TRANSACTION_COUNT_C_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_b, length) 
    names(TRANSACTION_COUNT_C_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_C_b')
    
    TRANSACTION_COUNT_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_l, length) 
    names(TRANSACTION_COUNT_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_l')
    
    TRANSACTION_COUNT_L_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_l, length) 
    names(TRANSACTION_COUNT_L_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_L_l')
    
    TRANSACTION_COUNT_C_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_l, length) 
    names(TRANSACTION_COUNT_C_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'TRANSACTION_COUNT_C_l')
    
    # 2. AVG_BET_SIZE
    AVG_BET_SIZE_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_b, mean, na.rm=T) 
    names(AVG_BET_SIZE_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_b')
    
    AVG_BET_SIZE_L_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_b, mean, na.rm=T) 
    names(AVG_BET_SIZE_L_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_L_b')
    
    AVG_BET_SIZE_C_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_b, mean, na.rm=T) 
    names(AVG_BET_SIZE_C_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_C_b')
    
    AVG_BET_SIZE_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_l, mean, na.rm=T) 
    names(AVG_BET_SIZE_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_l')
    
    AVG_BET_SIZE_L_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_l, mean, na.rm=T) 
    names(AVG_BET_SIZE_L_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_L_l')
    
    AVG_BET_SIZE_C_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_l, mean, na.rm=T) 
    names(AVG_BET_SIZE_C_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_C_l')
    
    # 3. MAX_BET_SIZE
    MAX_BET_SIZE_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_b, max, na.rm=T) 
    names(MAX_BET_SIZE_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_b')
    
    MAX_BET_SIZE_L_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_b, max, na.rm=T) 
    names(MAX_BET_SIZE_L_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_L_b')
    
    MAX_BET_SIZE_C_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_b, max, na.rm=T) 
    names(MAX_BET_SIZE_C_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_C_b')
    
    MAX_BET_SIZE_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_l, max, na.rm=T) 
    names(MAX_BET_SIZE_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_l')
    
    MAX_BET_SIZE_L_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_l, max, na.rm=T) 
    names(MAX_BET_SIZE_L_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_L_l')
    
    MAX_BET_SIZE_C_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_l, max, na.rm=T) 
    names(MAX_BET_SIZE_C_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_C_l')
    
    # 4. MIN_BET_SIZE
    MIN_BET_SIZE_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_b, min, na.rm=T) 
    names(MIN_BET_SIZE_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_b')
    
    MIN_BET_SIZE_L_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_b, min, na.rm=T) 
    names(MIN_BET_SIZE_L_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_L_b')
    
    MIN_BET_SIZE_C_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_b, min, na.rm=T) 
    names(MIN_BET_SIZE_C_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_C_b')
    
    MIN_BET_SIZE_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_l, min, na.rm=T) 
    names(MIN_BET_SIZE_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_l')
    
    MIN_BET_SIZE_L_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_l, min, na.rm=T) 
    names(MIN_BET_SIZE_L_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_L_l')
    
    MIN_BET_SIZE_C_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_l, min, na.rm=T) 
    names(MIN_BET_SIZE_C_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_C_l')
    
    # 5. STDEV_BET_SIZE
    STDEV_BET_SIZE_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_b, sd, na.rm=T) 
    names(STDEV_BET_SIZE_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_b')
    STDEV_BET_SIZE_b[is.na(STDEV_BET_SIZE_b$STDEV_BET_SIZE_b), 5] <- 0
    
    STDEV_BET_SIZE_L_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_b, sd, na.rm=T) 
    names(STDEV_BET_SIZE_L_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_L_b')
    STDEV_BET_SIZE_L_b[is.na(STDEV_BET_SIZE_L_b$STDEV_BET_SIZE_L_b), 5] <- 0
    
    STDEV_BET_SIZE_C_b <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_b, sd, na.rm=T) 
    names(STDEV_BET_SIZE_C_b) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_C_b')
    STDEV_BET_SIZE_C_b[is.na(STDEV_BET_SIZE_C_b$STDEV_BET_SIZE_C_b), 5] <- 0
    
    STDEV_BET_SIZE_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d_l, sd, na.rm=T) 
    names(STDEV_BET_SIZE_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_l')
    STDEV_BET_SIZE_l[is.na(STDEV_BET_SIZE_l$STDEV_BET_SIZE_l), 5] <- 0
    
    STDEV_BET_SIZE_L_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl_l, sd, na.rm=T) 
    names(STDEV_BET_SIZE_L_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_L_l')
    STDEV_BET_SIZE_L_l[is.na(STDEV_BET_SIZE_L_l$STDEV_BET_SIZE_L_l), 5] <- 0
    
    STDEV_BET_SIZE_C_l <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc_l, sd, na.rm=T) 
    names(STDEV_BET_SIZE_C_l) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_C_l')
    STDEV_BET_SIZE_C_l[is.na(STDEV_BET_SIZE_C_l$STDEV_BET_SIZE_C_l), 5] <- 0
    
    # 6. AVG_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    AVG_PLACED_TAKEN_TIME_b <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b, mean, na.rm=T) #EVENT_ID
    names(AVG_PLACED_TAKEN_TIME_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_PLACED_TAKEN_TIME_b')
    
    AVG_PLACED_TAKEN_TIME_l <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l, mean, na.rm=T) #EVENT_ID
    names(AVG_PLACED_TAKEN_TIME_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_PLACED_TAKEN_TIME_l')
    
    # 7. STDEV_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    STDEV_PLACED_TAKEN_TIME_b <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b, sd, na.rm=T) 
    names(STDEV_PLACED_TAKEN_TIME_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_PLACED_TAKEN_TIME_b') #EVENT_ID
    STDEV_PLACED_TAKEN_TIME_b[is.na(STDEV_PLACED_TAKEN_TIME_b$STDEV_PLACED_TAKEN_TIME_b), 5] <- 0
    
    STDEV_PLACED_TAKEN_TIME_l <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l, sd, na.rm=T) 
    names(STDEV_PLACED_TAKEN_TIME_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_PLACED_TAKEN_TIME_l') #EVENT_ID
    STDEV_PLACED_TAKEN_TIME_l[is.na(STDEV_PLACED_TAKEN_TIME_l$STDEV_PLACED_TAKEN_TIME_l), 5] <- 0
    
    # 8. AVG_TAKEN_HOUR (?)
    AVG_TAKEN_HOUR_b <- aggregate(as.numeric(format(PLACED_DATE, "%H")) ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b, mean, na.rm=T) #EVENT_ID
    names(AVG_TAKEN_HOUR_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_TAKEN_HOUR_b')
    
    AVG_TAKEN_HOUR_l <- aggregate(as.numeric(format(PLACED_DATE, "%H")) ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l, mean, na.rm=T) #EVENT_ID
    names(AVG_TAKEN_HOUR_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_TAKEN_HOUR_l')
    
    # 9. PREV_WIN_RATE (*)
    PREV_WIN_RATE_W_b <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b[d_b$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A_b <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b, length) 
    
    PREV_WIN_RATE_W_A_b <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d_b[d_b$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A_A_b <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d_b, length) 
    
    REV_WIN_RATE_b <- merge(PREV_WIN_RATE_W_b, PREV_WIN_RATE_A_b, all = T, by = c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET'))
    names(REV_WIN_RATE_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'WIN_b', 'ALL_b')
    REV_WIN_RATE_b[is.na(REV_WIN_RATE_b$WIN_b), 'WIN_b'] <- 0
    REV_WIN_RATE_b$REV_WIN_RATE_b <- REV_WIN_RATE_b$WIN_b/REV_WIN_RATE_b$ALL_b
    REV_WIN_RATE_b$WIN_b <- NULL; REV_WIN_RATE_b$ALL_b <- NULL
    
    REV_WIN_RATE_ALL_b <- merge(PREV_WIN_RATE_W_A_b, PREV_WIN_RATE_A_A_b, all = T, by = c('ACCOUNT_ID', 'STATUS_ID'))
    names(REV_WIN_RATE_ALL_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'WIN_b', 'ALL_b')
    REV_WIN_RATE_ALL_b[is.na(REV_WIN_RATE_ALL_b$WIN_b), 'WIN_b'] <- 0
    REV_WIN_RATE_ALL_b$REV_WIN_RATE_ALL_b <- REV_WIN_RATE_ALL_b$WIN_b/REV_WIN_RATE_ALL_b$ALL_b
    REV_WIN_RATE_ALL_b$WIN_b <- NULL; REV_WIN_RATE_ALL_b$ALL_b <- NULL
    
    PREV_WIN_RATE_b <- merge(REV_WIN_RATE_b, REV_WIN_RATE_ALL_b, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'STATUS_ID'))
    #
    PREV_WIN_RATE_W_l <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l[d_l$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A_l <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l, length) 
    
    PREV_WIN_RATE_W_A_l <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d_l[d_l$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A_A_l <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d_l, length) 
    
    REV_WIN_RATE_l <- merge(PREV_WIN_RATE_W_l, PREV_WIN_RATE_A_l, all = T, by = c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET'))
    names(REV_WIN_RATE_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'WIN_l', 'ALL_l')
    REV_WIN_RATE_l[is.na(REV_WIN_RATE_l$WIN_l), 'WIN_l'] <- 0
    REV_WIN_RATE_l$REV_WIN_RATE_l <- REV_WIN_RATE_l$WIN_l/REV_WIN_RATE_l$ALL_l
    REV_WIN_RATE_l$WIN_l <- NULL; REV_WIN_RATE_l$ALL_l <- NULL
    
    REV_WIN_RATE_ALL_l <- merge(PREV_WIN_RATE_W_A_l, PREV_WIN_RATE_A_A_l, all = T, by = c('ACCOUNT_ID', 'STATUS_ID'))
    names(REV_WIN_RATE_ALL_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'WIN_l', 'ALL_l')
    REV_WIN_RATE_ALL_l[is.na(REV_WIN_RATE_ALL_l$WIN_l), 'WIN_l'] <- 0
    REV_WIN_RATE_ALL_l$REV_WIN_RATE_ALL_l <- REV_WIN_RATE_ALL_l$WIN_l/REV_WIN_RATE_ALL_l$ALL_l
    REV_WIN_RATE_ALL_l$WIN_l <- NULL; REV_WIN_RATE_ALL_l$ALL_l <- NULL
    
    PREV_WIN_RATE_l <- merge(REV_WIN_RATE_l, REV_WIN_RATE_ALL_l, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'STATUS_ID'))
    
    # 10. NET_PROFIT (*)
    NET_PROFIT_b <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b, sum, na.rm=T)  #EVENT_ID
    names(NET_PROFIT_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'NET_PROFIT_b')
    
    NET_PROFIT_l <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l, sum, na.rm=T)  #EVENT_ID
    names(NET_PROFIT_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'NET_PROFIT_l')
    
    # 11. MARGIN (*)
    MARGIN_b <- aggregate(PRICE_TAKEN * BET_SIZE ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_b, sum, na.rm=T) 
    names(MARGIN_b) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'MARGIN_b') #EVENT_ID
    MARGIN_b$MARGIN_b <- NET_PROFIT_b$NET_PROFIT_b/MARGIN_b$MARGIN_b
    
    MARGIN_l <- aggregate(PRICE_TAKEN * BET_SIZE ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d_l, sum, na.rm=T) 
    names(MARGIN_l) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'MARGIN_l') #EVENT_ID
    MARGIN_l$MARGIN_l <- NET_PROFIT_l$NET_PROFIT_l/MARGIN_l$MARGIN_l
    
    # MERGE AND RETURN Member base
    mbr.event <- dt[!duplicated(paste0(dt$ACCOUNT_ID, dt$EVENT_ID)),c('ACCOUNT_ID', 'EVENT_ID')]
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_b[TRANSACTION_COUNT_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_b[TRANSACTION_COUNT_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L_b[TRANSACTION_COUNT_L_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L_b[TRANSACTION_COUNT_L_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C_b[TRANSACTION_COUNT_C_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C_b[TRANSACTION_COUNT_C_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_l[TRANSACTION_COUNT_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_l[TRANSACTION_COUNT_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L_l[TRANSACTION_COUNT_L_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L_l[TRANSACTION_COUNT_L_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C_l[TRANSACTION_COUNT_C_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C_l[TRANSACTION_COUNT_C_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_b[AVG_BET_SIZE_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_b[AVG_BET_SIZE_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L_b[AVG_BET_SIZE_L_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L_b[AVG_BET_SIZE_L_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C_b[AVG_BET_SIZE_C_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C_b[AVG_BET_SIZE_C_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_l[AVG_BET_SIZE_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_l[AVG_BET_SIZE_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L_l[AVG_BET_SIZE_L_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L_l[AVG_BET_SIZE_L_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C_l[AVG_BET_SIZE_C_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C_l[AVG_BET_SIZE_C_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_b[MAX_BET_SIZE_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_b[MAX_BET_SIZE_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L_b[MAX_BET_SIZE_L_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L_b[MAX_BET_SIZE_L_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C_b[MAX_BET_SIZE_C_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C_l[MAX_BET_SIZE_C_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_l[MAX_BET_SIZE_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_l[MAX_BET_SIZE_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L_l[MAX_BET_SIZE_L_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L_l[MAX_BET_SIZE_L_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C_l[MAX_BET_SIZE_C_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C_l[MAX_BET_SIZE_C_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_b[MIN_BET_SIZE_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_b[MIN_BET_SIZE_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L_b[MIN_BET_SIZE_L_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L_b[MIN_BET_SIZE_L_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C_b[MIN_BET_SIZE_C_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C_b[MIN_BET_SIZE_C_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_l[MIN_BET_SIZE_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_l[MIN_BET_SIZE_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L_l[MIN_BET_SIZE_L_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L_l[MIN_BET_SIZE_L_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C_l[MIN_BET_SIZE_C_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C_l[MIN_BET_SIZE_C_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_b[STDEV_BET_SIZE_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_b[STDEV_BET_SIZE_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L_b[STDEV_BET_SIZE_L_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L_b[STDEV_BET_SIZE_L_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C_b[STDEV_BET_SIZE_C_b$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C_b[STDEV_BET_SIZE_C_b$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_l[STDEV_BET_SIZE_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_l[STDEV_BET_SIZE_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L_l[STDEV_BET_SIZE_L_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L_l[STDEV_BET_SIZE_L_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C_l[STDEV_BET_SIZE_C_l$INPLAY_BET == 'Y', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C_l[STDEV_BET_SIZE_C_l$INPLAY_BET == 'N', c(1,2,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME_b[AVG_PLACED_TAKEN_TIME_b$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME_b[AVG_PLACED_TAKEN_TIME_b$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME_l[AVG_PLACED_TAKEN_TIME_l$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME_l[AVG_PLACED_TAKEN_TIME_l$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME_b[STDEV_PLACED_TAKEN_TIME_b$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME_b[STDEV_PLACED_TAKEN_TIME_b$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME_l[STDEV_PLACED_TAKEN_TIME_l$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME_l[STDEV_PLACED_TAKEN_TIME_l$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR_b[AVG_TAKEN_HOUR_b$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR_b[AVG_TAKEN_HOUR_b$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR_l[AVG_TAKEN_HOUR_l$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR_l[AVG_TAKEN_HOUR_l$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, PREV_WIN_RATE_b[PREV_WIN_RATE_b$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, PREV_WIN_RATE_b[PREV_WIN_RATE_b$INPLAY_BET == 'N', c(1,4,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, PREV_WIN_RATE_l[PREV_WIN_RATE_l$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, PREV_WIN_RATE_l[PREV_WIN_RATE_l$INPLAY_BET == 'N', c(1,4,5)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, NET_PROFIT_b[NET_PROFIT_b$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, NET_PROFIT_b[NET_PROFIT_b$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, NET_PROFIT_l[NET_PROFIT_l$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, NET_PROFIT_l[NET_PROFIT_l$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, MARGIN_b[MARGIN_b$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, MARGIN_b[MARGIN_b$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, MARGIN_l[MARGIN_l$INPLAY_BET == 'Y', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, MARGIN_l[MARGIN_l$INPLAY_BET == 'N', c(1,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    names(mbr.event) <- c('ACCOUNT_ID','EVENT_ID',
                          'TRANSACTION_COUNT_INPLAY_b','TRANSACTION_COUNT_OUTPLAY_b','TRANSACTION_COUNT_INPLAY_L_b','TRANSACTION_COUNT_OUTPLAY_L_b','TRANSACTION_COUNT_INPLAY_C_b','TRANSACTION_COUNT_OUTPLAY_C_b',
                          'TRANSACTION_COUNT_INPLAY_l','TRANSACTION_COUNT_OUTPLAY_l','TRANSACTION_COUNT_INPLAY_L_l','TRANSACTION_COUNT_OUTPLAY_L_l','TRANSACTION_COUNT_INPLAY_C_l','TRANSACTION_COUNT_OUTPLAY_C_l',
                          
                          'AVG_BET_SIZE_INPLAY_b','AVG_BET_SIZE_OUTPLAY_b','AVG_BET_SIZE_INPLAY_L_b','AVG_BET_SIZE_OUTPLAY_L_b','AVG_BET_SIZE_INPLAY_C_b','AVG_BET_SIZE_OUTPLAY_C_b',
                          'AVG_BET_SIZE_INPLAY_l','AVG_BET_SIZE_OUTPLAY_l','AVG_BET_SIZE_INPLAY_L_l','AVG_BET_SIZE_OUTPLAY_L_l','AVG_BET_SIZE_INPLAY_C_l','AVG_BET_SIZE_OUTPLAY_C_l',
                          
                          'MAX_BET_SIZE_INPLAY_b','MAX_BET_SIZE_OUTPLAY_b','MAX_BET_SIZE_INPLAY_L_b','MAX_BET_SIZE_OUTPLAY_L_b','MAX_BET_SIZE_INPLAY_C_b','MAX_BET_SIZE_OUTPLAY_C_b',
                          'MAX_BET_SIZE_INPLAY_l','MAX_BET_SIZE_OUTPLAY_l','MAX_BET_SIZE_INPLAY_L_l','MAX_BET_SIZE_OUTPLAY_L_l','MAX_BET_SIZE_INPLAY_C_l','MAX_BET_SIZE_OUTPLAY_C_l',
                          
                          'MIN_BET_SIZE_INPLAY_b','MIN_BET_SIZE_OUTPLAY_b','MIN_BET_SIZE_INPLAY_L_b','MIN_BET_SIZE_OUTPLAY_L_b','MIN_BET_SIZE_INPLAY_C_b','MIN_BET_SIZE_OUTPLAY_C_b',
                          'MIN_BET_SIZE_INPLAY_l','MIN_BET_SIZE_OUTPLAY_l','MIN_BET_SIZE_INPLAY_L_l','MIN_BET_SIZE_OUTPLAY_L_l','MIN_BET_SIZE_INPLAY_C_l','MIN_BET_SIZE_OUTPLAY_C_l',
                          
                          'STDEV_BET_SIZE_INPLAY_b','STDEV_BET_SIZE_OUTPLAY_b','STDEV_BET_SIZE_INPLAY_L_b','STDEV_BET_SIZE_OUTPLAY_L_b','STDEV_BET_SIZE_INPLAY_C_b','STDEV_BET_SIZE_OUTPLAY_C_b',
                          'STDEV_BET_SIZE_INPLAY_l','STDEV_BET_SIZE_OUTPLAY_l','STDEV_BET_SIZE_INPLAY_L_l','STDEV_BET_SIZE_OUTPLAY_L_l','STDEV_BET_SIZE_INPLAY_C_l','STDEV_BET_SIZE_OUTPLAY_C_l',
                          
                          'AVG_PLACED_TAKEN_TIME_INPLAY_b','AVG_PLACED_TAKEN_TIME_OUTPLAY_b',
                          'AVG_PLACED_TAKEN_TIME_INPLAY_l','AVG_PLACED_TAKEN_TIME_OUTPLAY_l',
                          
                          'STDEV_PLACED_TAKEN_TIME_INPLAY_b','STDEV_PLACED_TAKEN_TIME_OUTPLAY_b',
                          'STDEV_PLACED_TAKEN_TIME_INPLAY_l','STDEV_PLACED_TAKEN_TIME_OUTPLAY_l',
                          
                          'AVG_TAKEN_HOUR_INPLAY_b','AVG_TAKEN_HOUR_OUTPLAY_b',
                          'AVG_TAKEN_HOUR_INPLAY_l','AVG_TAKEN_HOUR_OUTPLAY_l',
                          
                          'PREV_WIN_RATE_INPLAY_b','PREV_WIN_RATE_OUTPLAY_b','PREV_WIN_RATE_b',
                          'PREV_WIN_RATE_INPLAY_l','PREV_WIN_RATE_OUTPLAY_l','PREV_WIN_RATE_l',
                          
                          'NET_PROFIT_INPLAY_b','NET_PROFIT_OUTPLAY_b',
                          'NET_PROFIT_INPLAY_l','NET_PROFIT_OUTPLAY_l',
                          
                          'MARGIN_INPLAY_b','MARGIN_OUTPLAY_b',
                          'MARGIN_INPLAY_l','MARGIN_OUTPLAY_l'
    )
    mbr.event[, -c(1,2,71:80)][is.na(mbr.event[, -c(1,2,71:80)])] <- 0
    mbr.event[, c(75:80)][is.na(mbr.event[, c(75:80)])] <- 0.5
    mbr.event$AVG_TAKEN_HOUR_INPLAY_b <- floor(mbr.event$AVG_TAKEN_HOUR_INPLAY_b)
    mbr.event$AVG_TAKEN_HOUR_OUTPLAY_b <- floor(mbr.event$AVG_TAKEN_HOUR_OUTPLAY_b)
    mbr.event$AVG_TAKEN_HOUR_INPLAY_l <- floor(mbr.event$AVG_TAKEN_HOUR_INPLAY_l)
    mbr.event$AVG_TAKEN_HOUR_OUTPLAY_l <- floor(mbr.event$AVG_TAKEN_HOUR_OUTPLAY_l)
    
    # 10. CANCEL_RATIO
    mbr.event$CANCEL_RATIO_INPLAY_b <- mbr.event$TRANSACTION_COUNT_INPLAY_C_b/(mbr.event$TRANSACTION_COUNT_INPLAY_C_b+mbr.event$TRANSACTION_COUNT_INPLAY_b+mbr.event$TRANSACTION_COUNT_INPLAY_L_b)
    mbr.event$CANCEL_RATIO_OUTPLAY_b <- mbr.event$TRANSACTION_COUNT_OUTPLAY_C_b/(mbr.event$TRANSACTION_COUNT_OUTPLAY_C_b+mbr.event$TRANSACTION_COUNT_OUTPLAY_b+mbr.event$TRANSACTION_COUNT_OUTPLAY_L_b)
    mbr.event$CANCEL_RATIO_INPLAY_l <- mbr.event$TRANSACTION_COUNT_INPLAY_C_l/(mbr.event$TRANSACTION_COUNT_INPLAY_C_l+mbr.event$TRANSACTION_COUNT_INPLAY_l+mbr.event$TRANSACTION_COUNT_INPLAY_L_l)
    mbr.event$CANCEL_RATIO_OUTPLAY_l <- mbr.event$TRANSACTION_COUNT_OUTPLAY_C_l/(mbr.event$TRANSACTION_COUNT_OUTPLAY_C_l+mbr.event$TRANSACTION_COUNT_OUTPLAY_l+mbr.event$TRANSACTION_COUNT_OUTPLAY_L_l)
    
    # 11. INPLAY_RATIO
    mbr.event$INPLAY_RATIO_b <- mbr.event$TRANSACTION_COUNT_INPLAY_b/(mbr.event$TRANSACTION_COUNT_INPLAY_b + mbr.event$TRANSACTION_COUNT_OUTPLAY_b)
    mbr.event$INPLAY_RATIO_l <- mbr.event$TRANSACTION_COUNT_INPLAY_l/(mbr.event$TRANSACTION_COUNT_INPLAY_l + mbr.event$TRANSACTION_COUNT_OUTPLAY_l)
    
    mbr.event[,c(89:94)][is.na(mbr.event[,c(89:94)])] <- 0
    
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

save(mbr.event, file='data/mbr_event_data_BL.RData')

d2 <- read.csv('data/semi_and_final_features.csv', stringsAsFactors=FALSE,na.strings = "")
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
