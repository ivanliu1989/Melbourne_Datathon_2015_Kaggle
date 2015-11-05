library(dplyr)

# set the working directory
setwd("C:/R/Betfair/")

# trim function
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

preprocess_data <- function(data.all) {
  data.all$BID_TYP <- trim(data.all$BID_TYP)
  data.all$STATUS_ID <- trim(data.all$STATUS_ID)
  # Filter to only get settled transactions
  data.all <- filter(data.all, STATUS_ID == 'S')
  # Fix the profit calculation
  data.all$PROFIT_LOSS <- ifelse(data.all$PROFIT_LOSS == 0, 0, ifelse(data.all$PROFIT_LOSS > 0, 
         ifelse(data.all$BID_TYP=='B',(data.all$PRICE_TAKEN-1)*data.all$BET_SIZE, data.all$BET_SIZE), 
         ifelse(data.all$BID_TYP=='L',(data.all$PRICE_TAKEN-1)*-1*data.all$BET_SIZE, -1*data.all$BET_SIZE)))
  return (data.all)
}

# Save us from loading again if we have already saved a copy
if (file.exists('data_pre.rds')) {
  data.pre <- readRDS('data_pre.rds')
} else {
  data.1 <- read.csv("Datathon WC Data Games 1-10.csv", fileEncoding="UTF-8-BOM")
  data.2 <- read.csv("Datathon WC Data Games 11-20.csv", fileEncoding="UTF-8-BOM")
  data.3 <- read.csv("Datathon WC Data Games 21-30.csv", fileEncoding="UTF-8-BOM")
  data.4 <- read.csv("Datathon WC Data Games 31-40.csv", fileEncoding="UTF-8-BOM")
  data.5 <- read.csv("Datathon WC Data Games QTR Finals.csv", fileEncoding="UTF-8-BOM")
  data.all <- rbind(data.1, data.2, data.3, data.4, data.5)
  data.pre <- preprocess_data(data.all)
  saveRDS(data.pre, 'data_pre.rds')
}

# aggregate the values - calculate the 'score' as +1 if a win, -1 if a lose by each individual Account+Event.
acc_event <- summarise(group_by(data.pre, ACCOUNT_ID, EVENT_ID), event_profit_loss = sum(PROFIT_LOSS, na.rm=TRUE))
acc <- summarise(group_by(acc_event, ACCOUNT_ID), Prediction = sum(ifelse(event_profit_loss > 0, 1, -1), na.rm=TRUE))

# read in the IDs to predict
valid_accs <- data.frame(ACCOUNT_ID = read.csv("sample_submission_bet_size.csv")[,1])
predictions <- left_join(valid_accs, acc, by = "ACCOUNT_ID")

# handle the new punters - assign them 0
predictions$Prediction[is.na(predictions$Prediction)] <- 0

# write the submission file
write.csv(predictions, 'past_history_game_benchmark.csv', row.names=FALSE)

