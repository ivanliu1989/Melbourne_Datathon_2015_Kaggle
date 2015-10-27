#--------------------------------------------------------------
# a sample submission for the DSM Datathon Kaggle Competition
#
# this is based on the past performance of the punters
#
# it will score 0.56570 on the public leaderboard
#--------------------------------------------------------------


## the location of the data
myDataFolder <- "H:\\DataScienceMelbourne\\datathon_data\\"

##sample submission file
submission_file <- 'sample_submission_bet_size.csv'

##the new submission file
out_file <- 'sample_submission_past_history.csv'

##the historical files
historical_files <-  c('Datathon WC Data Games 1-10.csv'
                      ,'Datathon WC Data Games 11-20.csv'
                      ,'Datathon WC Data Games 21-30.csv'
                      ,'Datathon WC Data Games 31-40.csv'
                      ,'Datathon WC Data Games QTR Finals.csv'
                      )



#append the root folder to the name
historical_files <- paste(myDataFolder,historical_files,sep="")
submission_file <- paste(myDataFolder,submission_file,sep="")
out_file <- paste(myDataFolder,out_file,sep="")

#read the submision file
sample_submission <-read.csv(submission_file)

#R seems to change the column names
colnames(sample_submission) <- c('ACCOUNT_ID','Prediction')


#read in the historical data and filter
for (i in 1:length(historical_files)){

  #read in the data  
  d <- read.csv(historical_files[i], stringsAsFactors=FALSE,na.strings = "")
  
  #trim the white space
  d$STATUS_ID <- trimws(d$STATUS_ID)
  d$BID_TYP <- trimws(d$BID_TYP)
  
  #the columns of interest 
  requiredColumns <- c('ACCOUNT_ID','BID_TYP','PRICE_TAKEN','BET_SIZE','STATUS_ID','PROFIT_LOSS')
  
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


#now create the submission file  
submit <- merge(sample_submission,tot_profit_loss,all.x = TRUE,all.y = FALSE)

#those who never appeared before - give a zero
newPunters <- is.na(submit$PROFIT_LOSS1)
submit$PROFIT_LOSS1[newPunters] <- 0

#profit becomes the prediction
submit$Prediction <- submit$PROFIT_LOSS1
submit$PROFIT_LOSS1 <- NULL

#create the submission file
write.csv(submit,out_file,quote = FALSE,row.names = FALSE)