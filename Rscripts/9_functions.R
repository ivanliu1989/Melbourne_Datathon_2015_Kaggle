feat.eng <- function(d){
    #trim the white space
    d$STATUS_ID <- trimws(d$STATUS_ID)
    d$BID_TYP <- trimws(d$BID_TYP)
    
    #country list
    country <- d[!duplicated(d[,c('ACCOUNT_ID','COUNTRY_OF_RESIDENCE_NAME')]),c('ACCOUNT_ID','COUNTRY_OF_RESIDENCE_NAME')]
    country$COUNTRY_OF_RESIDENCE_NAME <- substr(country$COUNTRY_OF_RESIDENCE_NAME, 1,5) 
    country$COUNTRY_OF_RESIDENCE_NAME <- gsub(" ", "", country$COUNTRY_OF_RESIDENCE_NAME, fixed = TRUE)
    
    #the columns of interest 
    requiredColumns <- c('ACCOUNT_ID','EVENT_ID','BID_TYP','PRICE_TAKEN','BET_SIZE','STATUS_ID','PROFIT_LOSS','INPLAY_BET','PLACED_DATE','TAKEN_DATE')
    
    #rows of interest - just the settled bets
    requiredRows <- which(d$STATUS_ID == 'S')
    requiredRows2 <- which(d$STATUS_ID == 'L')
    requiredRows3 <- which(d$STATUS_ID == 'C')
    
    #filter
    test <- d[requiredRows, c('ACCOUNT_ID','EVENT_ID','BID_TYP', 'INPLAY_BET','BET_SIZE')]
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
    
    #date format
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
    AVG_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, mean, na.rm = T) 
    names(AVG_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE')
    
    AVG_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, mean, na.rm = T) 
    names(AVG_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_L')
    
    AVG_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, mean, na.rm = T) 
    names(AVG_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_BET_SIZE_C')
    
    # 3. MAX_BET_SIZE
    MAX_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, max, na.rm = T) 
    names(MAX_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE')
    
    MAX_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, max, na.rm = T) 
    names(MAX_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_L')
    
    MAX_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, max, na.rm = T) 
    names(MAX_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MAX_BET_SIZE_C')
    
    # 4. MIN_BET_SIZE
    MIN_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, min, na.rm = T) 
    names(MIN_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE')
    
    MIN_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, min, na.rm = T) 
    names(MIN_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_L')
    
    MIN_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, min, na.rm = T) 
    names(MIN_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'MIN_BET_SIZE_C')
    
    # 5. STDEV_BET_SIZE
    STDEV_BET_SIZE <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=d, sd, na.rm = T) 
    names(STDEV_BET_SIZE) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE')
    STDEV_BET_SIZE[is.na(STDEV_BET_SIZE$STDEV_BET_SIZE), 'STDEV_BET_SIZE'] <- 0
    
    STDEV_BET_SIZE_L <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dl, sd, na.rm = T) 
    names(STDEV_BET_SIZE_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_L')
    STDEV_BET_SIZE_L[is.na(STDEV_BET_SIZE_L$STDEV_BET_SIZE_L), 'STDEV_BET_SIZE_L'] <- 0
    
    STDEV_BET_SIZE_C <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + STATUS_ID + INPLAY_BET ,data=dc, sd, na.rm = T) 
    names(STDEV_BET_SIZE_C) <- c('ACCOUNT_ID', 'EVENT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_BET_SIZE_C')
    STDEV_BET_SIZE_C[is.na(STDEV_BET_SIZE_C$STDEV_BET_SIZE_C), 'STDEV_BET_SIZE_C'] <- 0
    
    # 6. AVG_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    AVG_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, mean, na.rm = T) #EVENT_ID
    names(AVG_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_PLACED_TAKEN_TIME')
    
    # 7. STDEV_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    STDEV_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sd, na.rm = T) 
    names(STDEV_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_PLACED_TAKEN_TIME') #EVENT_ID
    STDEV_PLACED_TAKEN_TIME[is.na(STDEV_PLACED_TAKEN_TIME$STDEV_PLACED_TAKEN_TIME), 'STDEV_PLACED_TAKEN_TIME'] <- 0
    
    # 8 SKEW_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    SKEW_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, skewness, na.rm = T) 
    names(SKEW_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'SKEW_PLACED_TAKEN_TIME') #EVENT_ID
    SKEW_PLACED_TAKEN_TIME[is.na(SKEW_PLACED_TAKEN_TIME$SKEW_PLACED_TAKEN_TIME), 'SKEW_PLACED_TAKEN_TIME'] <- 0
    
    # 9 KURT_PLACED_TAKEN_TIME (ALL/INDIVIDUAL) (?)
    KURT_PLACED_TAKEN_TIME <- aggregate(PLACED_TAKEN_TIME ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, kurtosis, na.rm = T) 
    names(KURT_PLACED_TAKEN_TIME) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'KURT_PLACED_TAKEN_TIME') #EVENT_ID
    KURT_PLACED_TAKEN_TIME[is.na(KURT_PLACED_TAKEN_TIME$KURT_PLACED_TAKEN_TIME), 'KURT_PLACED_TAKEN_TIME'] <- 0
    
    # 10. AVG_TAKEN_HOUR (?)
    AVG_TAKEN_HOUR <- aggregate(as.numeric(format(PLACED_DATE, "%H")) ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, mean, na.rm = T) #EVENT_ID
    names(AVG_TAKEN_HOUR) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'AVG_TAKEN_HOUR')
    
    # 11 STDEV_TAKEN_HOUR (?)
    STDEV_TAKEN_HOUR <- aggregate(as.numeric(format(PLACED_DATE, "%H")) ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sd, na.rm = T) #EVENT_ID
    names(STDEV_TAKEN_HOUR) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'STDEV_TAKEN_HOUR')
    
    # 12. PREV_WIN_RATE (*)
    PREV_WIN_RATE_W <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d[d$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, length) 
    
    PREV_WIN_RATE_W_A <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d[d$PROFIT_LOSS>0,], length) 
    PREV_WIN_RATE_A_A <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID ,data=d, length) 
    
    PREV_WIN_RATE <- merge(PREV_WIN_RATE_W, PREV_WIN_RATE_A, all = T, by = c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET'))
    names(PREV_WIN_RATE) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'WIN', 'ALL')
    PREV_WIN_RATE[is.na(PREV_WIN_RATE$WIN), 'WIN'] <- 0
    PREV_WIN_RATE$PREV_WIN_RATE <- PREV_WIN_RATE$WIN/PREV_WIN_RATE$ALL
    PREV_WIN_RATE$WIN <- NULL; PREV_WIN_RATE$ALL <- NULL
    
    PREV_WIN_RATE_ALL <- merge(PREV_WIN_RATE_W_A, PREV_WIN_RATE_A_A, all = T, by = c('ACCOUNT_ID', 'STATUS_ID'))
    names(PREV_WIN_RATE_ALL) <- c('ACCOUNT_ID', 'STATUS_ID', 'WIN', 'ALL')
    PREV_WIN_RATE_ALL[is.na(PREV_WIN_RATE_ALL$WIN), 'WIN'] <- 0
    PREV_WIN_RATE_ALL$PREV_WIN_RATE_ALL <- PREV_WIN_RATE_ALL$WIN/PREV_WIN_RATE_ALL$ALL
    PREV_WIN_RATE_ALL$WIN <- NULL; PREV_WIN_RATE_ALL$ALL <- NULL
    
    PREV_WIN_RATE <- merge(PREV_WIN_RATE, PREV_WIN_RATE_ALL, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'STATUS_ID'))
    
    # 13. NET_PROFIT (*)
    NET_PROFIT <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sum, na.rm = T)  #EVENT_ID
    names(NET_PROFIT) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'NET_PROFIT')
    
    # 14. MARGIN (*)
    MARGIN <- aggregate(PRICE_TAKEN * BET_SIZE ~ ACCOUNT_ID + STATUS_ID + INPLAY_BET ,data=d, sum, na.rm = T) 
    names(MARGIN) <- c('ACCOUNT_ID', 'STATUS_ID', 'INPLAY_BET', 'MARGIN') #EVENT_ID
    MARGIN$MARGIN <- NET_PROFIT$NET_PROFIT/MARGIN$MARGIN
    
    # MERGE AND RETURN Member base
    mbr.event <- d[!duplicated(paste0(d$ACCOUNT_ID, d$EVENT_ID)),c('ACCOUNT_ID', 'EVENT_ID')]
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT[TRANSACTION_COUNT$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT[TRANSACTION_COUNT$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L[TRANSACTION_COUNT_L$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_L[TRANSACTION_COUNT_L$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C[TRANSACTION_COUNT_C$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, TRANSACTION_COUNT_C[TRANSACTION_COUNT_C$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'TRANSACTION_COUNT_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_BET_SIZE[AVG_BET_SIZE$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE[AVG_BET_SIZE$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, AVG_BET_SIZE_L[AVG_BET_SIZE_L$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_L[AVG_BET_SIZE_L$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, AVG_BET_SIZE_C[AVG_BET_SIZE_C$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, AVG_BET_SIZE_C[AVG_BET_SIZE_C$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'AVG_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, MAX_BET_SIZE[MAX_BET_SIZE$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE[MAX_BET_SIZE$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, MAX_BET_SIZE_L[MAX_BET_SIZE_L$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_L[MAX_BET_SIZE_L$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, MAX_BET_SIZE_C[MAX_BET_SIZE_C$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MAX_BET_SIZE_C[MAX_BET_SIZE_C$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'MAX_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, MIN_BET_SIZE[MIN_BET_SIZE$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE[MIN_BET_SIZE$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, MIN_BET_SIZE_L[MIN_BET_SIZE_L$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_L[MIN_BET_SIZE_L$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, MIN_BET_SIZE_C[MIN_BET_SIZE_C$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, MIN_BET_SIZE_C[MIN_BET_SIZE_C$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'MIN_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE[STDEV_BET_SIZE$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE[STDEV_BET_SIZE$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L[STDEV_BET_SIZE_L$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_L[STDEV_BET_SIZE_L$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE_L')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    # mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C[STDEV_BET_SIZE_C$INPLAY_BET == 'Y', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    mbr.event <- merge(mbr.event, STDEV_BET_SIZE_C[STDEV_BET_SIZE_C$INPLAY_BET == 'N', c('ACCOUNT_ID', 'EVENT_ID', 'STDEV_BET_SIZE_C')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME[AVG_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_PLACED_TAKEN_TIME[AVG_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c('ACCOUNT_ID','AVG_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME[STDEV_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, STDEV_PLACED_TAKEN_TIME[STDEV_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c('ACCOUNT_ID','STDEV_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, SKEW_PLACED_TAKEN_TIME[SKEW_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c('ACCOUNT_ID','SKEW_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, SKEW_PLACED_TAKEN_TIME[SKEW_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c('ACCOUNT_ID','SKEW_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, KURT_PLACED_TAKEN_TIME[KURT_PLACED_TAKEN_TIME$INPLAY_BET == 'Y', c('ACCOUNT_ID','KURT_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, KURT_PLACED_TAKEN_TIME[KURT_PLACED_TAKEN_TIME$INPLAY_BET == 'N', c('ACCOUNT_ID','KURT_PLACED_TAKEN_TIME')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR[AVG_TAKEN_HOUR$INPLAY_BET == 'Y', c('ACCOUNT_ID','AVG_TAKEN_HOUR')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, AVG_TAKEN_HOUR[AVG_TAKEN_HOUR$INPLAY_BET == 'N', c('ACCOUNT_ID','AVG_TAKEN_HOUR')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, STDEV_TAKEN_HOUR[STDEV_TAKEN_HOUR$INPLAY_BET == 'Y', c('ACCOUNT_ID','STDEV_TAKEN_HOUR')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, STDEV_TAKEN_HOUR[STDEV_TAKEN_HOUR$INPLAY_BET == 'N', c('ACCOUNT_ID','STDEV_TAKEN_HOUR')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, PREV_WIN_RATE[PREV_WIN_RATE$INPLAY_BET == 'Y', c('ACCOUNT_ID','PREV_WIN_RATE')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, PREV_WIN_RATE[PREV_WIN_RATE$INPLAY_BET == 'N', c('ACCOUNT_ID','PREV_WIN_RATE','PREV_WIN_RATE_ALL')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, NET_PROFIT[NET_PROFIT$INPLAY_BET == 'Y', c('ACCOUNT_ID','NET_PROFIT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, NET_PROFIT[NET_PROFIT$INPLAY_BET == 'N', c('ACCOUNT_ID','NET_PROFIT')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    mbr.event <- merge(mbr.event, MARGIN[MARGIN$INPLAY_BET == 'Y', c('ACCOUNT_ID','MARGIN')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, MARGIN[MARGIN$INPLAY_BET == 'N', c('ACCOUNT_ID','MARGIN')], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    names(mbr.event) <- c('ACCOUNT_ID','EVENT_ID',
                          'TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
                          'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
                          'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
                          'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
                          'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C',
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
    mbr.event[, c('TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
                  'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
                  'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
                  'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
                  'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C',
                  'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY',
                  'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY',
                  'SKEW_PLACED_TAKEN_TIME_INPLAY','SKEW_PLACED_TAKEN_TIME_OUTPLAY',
                  'KURT_PLACED_TAKEN_TIME_INPLAY','KURT_PLACED_TAKEN_TIME_OUTPLAY',
                  'STDEV_TAKEN_HOUR_INPLAY','STDEV_TAKEN_HOUR_OUTPLAY',
                  'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY',
                  'MARGIN_INPLAY','MARGIN_OUTPLAY')][is.na(mbr.event[, c('TRANSACTION_COUNT_INPLAY','TRANSACTION_COUNT_OUTPLAY','TRANSACTION_COUNT_OUTPLAY_L','TRANSACTION_COUNT_OUTPLAY_C',
                                                                         'AVG_BET_SIZE_INPLAY','AVG_BET_SIZE_OUTPLAY','AVG_BET_SIZE_OUTPLAY_L','AVG_BET_SIZE_OUTPLAY_C',
                                                                         'MAX_BET_SIZE_INPLAY','MAX_BET_SIZE_OUTPLAY','MAX_BET_SIZE_OUTPLAY_L','MAX_BET_SIZE_OUTPLAY_C',
                                                                         'MIN_BET_SIZE_INPLAY','MIN_BET_SIZE_OUTPLAY','MIN_BET_SIZE_OUTPLAY_L','MIN_BET_SIZE_OUTPLAY_C',
                                                                         'STDEV_BET_SIZE_INPLAY','STDEV_BET_SIZE_OUTPLAY','STDEV_BET_SIZE_OUTPLAY_L','STDEV_BET_SIZE_OUTPLAY_C',
                                                                         'AVG_PLACED_TAKEN_TIME_INPLAY','AVG_PLACED_TAKEN_TIME_OUTPLAY',
                                                                         'STDEV_PLACED_TAKEN_TIME_INPLAY','STDEV_PLACED_TAKEN_TIME_OUTPLAY',
                                                                         'SKEW_PLACED_TAKEN_TIME_INPLAY','SKEW_PLACED_TAKEN_TIME_OUTPLAY',
                                                                         'KURT_PLACED_TAKEN_TIME_INPLAY','KURT_PLACED_TAKEN_TIME_OUTPLAY',
                                                                         'STDEV_TAKEN_HOUR_INPLAY','STDEV_TAKEN_HOUR_OUTPLAY',
                                                                         'NET_PROFIT_INPLAY','NET_PROFIT_OUTPLAY',
                                                                         'MARGIN_INPLAY','MARGIN_OUTPLAY')])] <- 0
    mbr.event[, c('PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE')][is.na(mbr.event[, c('PREV_WIN_RATE_INPLAY','PREV_WIN_RATE_OUTPLAY','PREV_WIN_RATE')])] <- 0.5
    mbr.event$AVG_TAKEN_HOUR_INPLAY <- floor(mbr.event$AVG_TAKEN_HOUR_INPLAY)
    mbr.event$AVG_TAKEN_HOUR_OUTPLAY <- floor(mbr.event$AVG_TAKEN_HOUR_OUTPLAY)
    
    # 15. CANCEL_RATIO
    mbr.event$CANCEL_RATIO_OUTPLAY <- mbr.event$TRANSACTION_COUNT_OUTPLAY_C/(mbr.event$TRANSACTION_COUNT_OUTPLAY_C+mbr.event$TRANSACTION_COUNT_OUTPLAY+mbr.event$TRANSACTION_COUNT_OUTPLAY_L)
    
    # 16. INPLAY_RATIO
    mbr.event$INPLAY_RATIO <- mbr.event$TRANSACTION_COUNT_INPLAY/(mbr.event$TRANSACTION_COUNT_INPLAY + mbr.event$TRANSACTION_COUNT_OUTPLAY)
    mbr.event[,c(42:43)][is.na(mbr.event[,c(42:43)])] <- 0
    
    # 17. Country
    mbr.event <- merge(mbr.event, country, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    
    # 18. BL_RATIO
    BL_RATIO_INPLAY_B <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + BID_TYP + INPLAY_BET ,data=d[d$BID_TYP == 'B' & d$INPLAY_BET == 'Y',], length)
    names(BL_RATIO_INPLAY_B) <- c('ACCOUNT_ID', 'EVENT_ID', 'BID_TYP', 'INPLAY_BET', 'TRANS_B')
    BL_RATIO_INPLAY_L <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + BID_TYP + INPLAY_BET ,data=d[d$BID_TYP == 'L' & d$INPLAY_BET == 'Y',], length) 
    names(BL_RATIO_INPLAY_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'BID_TYP', 'INPLAY_BET', 'TRANS_L')
    BL_RATIO_INPLAY <- merge(BL_RATIO_INPLAY_B, BL_RATIO_INPLAY_L, all.x = TRUE, all.y = TRUE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    BL_RATIO_INPLAY[,c('TRANS_L', 'TRANS_B')][is.na(BL_RATIO_INPLAY[,c('TRANS_L', 'TRANS_B')])] <- 0
    BL_RATIO_INPLAY$BL_RATIO_INPLAY <- BL_RATIO_INPLAY$TRANS_L / (BL_RATIO_INPLAY$TRANS_B + BL_RATIO_INPLAY$TRANS_L)
    BL_RATIO_INPLAY <- aggregate(BL_RATIO_INPLAY ~ ACCOUNT_ID,data=BL_RATIO_INPLAY, mean, na.rm=T)
    
    BL_RATIO_OUTPLAY_B <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + BID_TYP + INPLAY_BET ,data=d[d$BID_TYP == 'B' & d$INPLAY_BET == 'N',], length)
    names(BL_RATIO_OUTPLAY_B) <- c('ACCOUNT_ID', 'EVENT_ID', 'BID_TYP', 'INPLAY_BET', 'TRANS_B')
    BL_RATIO_OUTPLAY_L <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + BID_TYP + INPLAY_BET ,data=d[d$BID_TYP == 'L' & d$INPLAY_BET == 'N',], length) 
    names(BL_RATIO_OUTPLAY_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'BID_TYP', 'INPLAY_BET', 'TRANS_L')
    BL_RATIO_OUTPLAY <- merge(BL_RATIO_OUTPLAY_B, BL_RATIO_OUTPLAY_L, all.x = TRUE, all.y = TRUE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    BL_RATIO_OUTPLAY[,c('TRANS_L', 'TRANS_B')][is.na(BL_RATIO_OUTPLAY[,c('TRANS_L', 'TRANS_B')])] <- 0
    BL_RATIO_OUTPLAY$BL_RATIO_OUTPLAY <- BL_RATIO_OUTPLAY$TRANS_L / (BL_RATIO_OUTPLAY$TRANS_B + BL_RATIO_OUTPLAY$TRANS_L)
    BL_RATIO_OUTPLAY <- aggregate(BL_RATIO_OUTPLAY ~ ACCOUNT_ID,data=BL_RATIO_OUTPLAY, mean, na.rm=T)
    
    BL_RATIO_B <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + BID_TYP + INPLAY_BET ,data=d[d$BID_TYP == 'B',], length)
    names(BL_RATIO_B) <- c('ACCOUNT_ID', 'EVENT_ID', 'BID_TYP', 'INPLAY_BET', 'TRANS_B')
    BL_RATIO_L <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID + BID_TYP + INPLAY_BET ,data=d[d$BID_TYP == 'L',], length) 
    names(BL_RATIO_L) <- c('ACCOUNT_ID', 'EVENT_ID', 'BID_TYP', 'INPLAY_BET', 'TRANS_L')
    BL_RATIO <- merge(BL_RATIO_B, BL_RATIO_L, all.x = TRUE, all.y = TRUE, by = c('ACCOUNT_ID', 'EVENT_ID'))
    BL_RATIO[,c('TRANS_L', 'TRANS_B')][is.na(BL_RATIO[,c('TRANS_L', 'TRANS_B')])] <- 0
    BL_RATIO$BL_RATIO <- BL_RATIO$TRANS_L / (BL_RATIO$TRANS_B + BL_RATIO$TRANS_L)
    BL_RATIO <- aggregate(BL_RATIO ~ ACCOUNT_ID,data=BL_RATIO, mean, na.rm=T)
    
    mbr.event <- merge(mbr.event, BL_RATIO_INPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, BL_RATIO_OUTPLAY, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event <- merge(mbr.event, BL_RATIO, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    mbr.event[is.na(mbr.event$BL_RATIO_INPLAY),'BL_RATIO_INPLAY'] <- mean(mbr.event$BL_RATIO_INPLAY, na.rm=T)
    mbr.event[is.na(mbr.event$BL_RATIO_OUTPLAY),'BL_RATIO_OUTPLAY'] <- mean(mbr.event$BL_RATIO_OUTPLAY, na.rm=T)
    mbr.event[is.na(mbr.event$BL_RATIO),'BL_RATIO'] <- mean(mbr.event$BL_RATIO, na.rm=T)
    
    # 14. B_L_DIFF
        TRANSACTION_COUNT_I <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='Y',], length)
        names(TRANSACTION_COUNT_I) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','TRANSACTION_COUNT_I')
        TRANSACTION_COUNT_O <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='N',], length)
        names(TRANSACTION_COUNT_O) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','TRANSACTION_COUNT_O')
        AVG_BET_SIZE_I <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='Y',], mean, na.rm = T)
        names(AVG_BET_SIZE_I) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','AVG_BET_SIZE_I')
        AVG_BET_SIZE_O <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='N',], mean, na.rm = T)
        names(AVG_BET_SIZE_O) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','AVG_BET_SIZE_O')
        MAX_BET_SIZE_I <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='Y',], max, na.rm = T)
        names(MAX_BET_SIZE_I) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','MAX_BET_SIZE_I')
        MAX_BET_SIZE_O <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='N',], max, na.rm = T)
        names(MAX_BET_SIZE_O) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','MAX_BET_SIZE_O')
        MIN_BET_SIZE_I <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='Y',], min, na.rm = T)
        names(MIN_BET_SIZE_I) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','MIN_BET_SIZE_I')
        MIN_BET_SIZE_O <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='N',], min, na.rm = T)
        names(MIN_BET_SIZE_O) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','MIN_BET_SIZE_O')
        STDEV_BET_SIZE_I <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='Y',], sd, na.rm = T)
        names(STDEV_BET_SIZE_I) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','STDEV_BET_SIZE_I')
        STDEV_BET_SIZE_O <- aggregate(BET_SIZE ~ ACCOUNT_ID + EVENT_ID + BID_TYP ,data=test[test$INPLAY_BET=='N',], sd, na.rm = T)
        names(STDEV_BET_SIZE_O) <- c('ACCOUNT_ID','EVENT_ID', 'BID_TYP','STDEV_BET_SIZE_O')
        
        test <- mbr.event[,c('ACCOUNT_ID', 'EVENT_ID')]
        
        test <- merge(test, TRANSACTION_COUNT_I[TRANSACTION_COUNT_I$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, TRANSACTION_COUNT_O[TRANSACTION_COUNT_O$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, AVG_BET_SIZE_I[AVG_BET_SIZE_I$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, AVG_BET_SIZE_O[AVG_BET_SIZE_O$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MAX_BET_SIZE_I[MAX_BET_SIZE_I$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MAX_BET_SIZE_O[MAX_BET_SIZE_O$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MIN_BET_SIZE_I[MIN_BET_SIZE_I$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MIN_BET_SIZE_O[MIN_BET_SIZE_O$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, STDEV_BET_SIZE_I[STDEV_BET_SIZE_I$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, STDEV_BET_SIZE_O[STDEV_BET_SIZE_O$BID_TYP=='B',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        
        test <- merge(test, TRANSACTION_COUNT_I[TRANSACTION_COUNT_I$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, TRANSACTION_COUNT_O[TRANSACTION_COUNT_O$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, AVG_BET_SIZE_I[AVG_BET_SIZE_I$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, AVG_BET_SIZE_O[AVG_BET_SIZE_O$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MAX_BET_SIZE_I[MAX_BET_SIZE_I$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MAX_BET_SIZE_O[MAX_BET_SIZE_O$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MIN_BET_SIZE_I[MIN_BET_SIZE_I$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, MIN_BET_SIZE_O[MIN_BET_SIZE_O$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, STDEV_BET_SIZE_I[STDEV_BET_SIZE_I$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        test <- merge(test, STDEV_BET_SIZE_O[STDEV_BET_SIZE_O$BID_TYP=='L',c(1,2,4)], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
        
        ### calculation
        test_feat_final <- test
        test[,-c(1,2)][is.na(test[,-c(1,2)])] <- 0
        test$BL_DIFF_TRANSACTION_COUNT_IN <- abs(test$TRANSACTION_COUNT_I.x - test$TRANSACTION_COUNT_I.y)/(test$TRANSACTION_COUNT_I.x + test$TRANSACTION_COUNT_I.y)
        test$BL_DIFF_AVG_BET_SIZE_IN<- abs(test$AVG_BET_SIZE_I.x - test$AVG_BET_SIZE_I.y)/(test$AVG_BET_SIZE_I.x + test$AVG_BET_SIZE_I.y)
        test$BL_DIFF_MAX_BET_SIZE_IN<- abs(test$MAX_BET_SIZE_I.x - test$MAX_BET_SIZE_I.y)/(test$MAX_BET_SIZE_I.x + test$MAX_BET_SIZE_I.y)
        test$BL_DIFF_MIN_BET_SIZE_IN<- abs(test$MIN_BET_SIZE_I.x - test$MIN_BET_SIZE_I.y)/(test$MIN_BET_SIZE_I.x + test$MIN_BET_SIZE_I.y)
        test$BL_DIFF_STDEV_BET_SIZE_IN<- abs(test$STDEV_BET_SIZE_I.x - test$STDEV_BET_SIZE_I.y)/(test$STDEV_BET_SIZE_I.x + test$STDEV_BET_SIZE_I.y)
        
        test$BL_DIFF_TRANSACTION_COUNT_OUT <- abs(test$TRANSACTION_COUNT_O.x - test$TRANSACTION_COUNT_O.y)/(test$TRANSACTION_COUNT_O.x + test$TRANSACTION_COUNT_O.y)
        test$BL_DIFF_AVG_BET_SIZE_OUT<- abs(test$AVG_BET_SIZE_O.x - test$AVG_BET_SIZE_O.y)/(test$AVG_BET_SIZE_O.x + test$AVG_BET_SIZE_O.y)
        test$BL_DIFF_MAX_BET_SIZE_OUT<- abs(test$MAX_BET_SIZE_O.x - test$MAX_BET_SIZE_O.y)/(test$MAX_BET_SIZE_O.x + test$MAX_BET_SIZE_O.y)
        test$BL_DIFF_MIN_BET_SIZE_OUT<- abs(test$MIN_BET_SIZE_O.x - test$MIN_BET_SIZE_O.y)/(test$MIN_BET_SIZE_O.x + test$MIN_BET_SIZE_O.y)
        test$BL_DIFF_STDEV_BET_SIZE_OUT<- abs(test$STDEV_BET_SIZE_O.x - test$STDEV_BET_SIZE_O.y)/(test$STDEV_BET_SIZE_O.x + test$STDEV_BET_SIZE_O.y)
        
        ### test_feat merge
        test_dt <- test[,c('ACCOUNT_ID', 'EVENT_ID','BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                                   'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
                                   'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                                   'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')]
        
        test_dt[,c('BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                   'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
                   'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                   'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')][is.na(test_dt[,c('BL_DIFF_TRANSACTION_COUNT_IN', 'BL_DIFF_AVG_BET_SIZE_IN',
                                                                                                                         'BL_DIFF_MAX_BET_SIZE_IN','BL_DIFF_MIN_BET_SIZE_IN','BL_DIFF_STDEV_BET_SIZE_IN',
                                                                                                                         'BL_DIFF_TRANSACTION_COUNT_OUT', 'BL_DIFF_AVG_BET_SIZE_OUT',
                                                                                                                         'BL_DIFF_MAX_BET_SIZE_OUT','BL_DIFF_MIN_BET_SIZE_OUT','BL_DIFF_STDEV_BET_SIZE_OUT')])] <- 0
        mbr.event <- merge(mbr.event, test_dt, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID', 'EVENT_ID'))
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