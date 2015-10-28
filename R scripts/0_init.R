setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(pROC); library(data.table)

## the location of the data
myDataFolder <- "../Datathon_Full_Dataset/"

##the historical files
historical_files <-  c('Datathon WC Data Games 1-10.csv'
                       ,'Datathon WC Data Games 11-20.csv'
                       ,'Datathon WC Data Games 21-30.csv'
                       ,'Datathon WC Data Games 31-40.csv'
                       ,'Datathon WC Data Games QTR Finals.csv'
)

#append the root folder to the name
historical_files <- paste(myDataFolder,historical_files,sep="")

#read in the historical data and filter
for (i in 1:length(historical_files)){
    
    #read in the data  
    d <- read.csv(historical_files[i], stringsAsFactors=FALSE, na.strings = "")
    
    #trim the white space
    d$STATUS_ID <- trimws(d$STATUS_ID)
    d$BID_TYP <- trimws(d$BID_TYP)
    
    #the columns of interest 
    requiredColumns <- c('ACCOUNT_ID','BID_TYP','PRICE_TAKEN','BET_SIZE','STATUS_ID','PROFIT_LOSS')
    # INPLAY_BET, PLACED_DATE-TAKEN_DATE, TAKEN_TIME, COUNTRY, TEAM_WIN_RATES, CUSTOMER_TYPE, PREV_WIN, PREV_AMOUNT
    
    #rows of interest - just the settled bets
    requiredRows <- which(d$STATUS_ID == 'S')
    
    #filter
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
    
    #aggregate profit by accountID
    this_profit_loss <- aggregate(PROFIT_LOSS1 ~ ACCOUNT_ID,data=d,sum)  
    
    #stack files
    if (i==1){
        tot_profit_loss <- this_profit_loss
    } else {
        tot_profit_loss <- rbind(tot_profit_loss,this_profit_loss)
    }
    
}

#aggregte again (over all files)
tot_profit_loss <- aggregate(PROFIT_LOSS1 ~ ACCOUNT_ID,data=tot_profit_loss,sum) 

summary(tot_profit_loss$PROFIT_LOSS1)
head(tot_profit_loss)
hist(tot_profit_loss$PROFIT_LOSS1,breaks=1000)