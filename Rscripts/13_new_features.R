setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); library(caret)
source('Rscripts/12_log_transformation.R')
load('data/1_complete_data_new.RData');
load('data/2_test_new.RData');ls()

total$win_hist <- ifelse(total$flag_regr > 0, 1, ifelse(total$flag_regr <0, -1, 0)) 
win_hist <- aggregate(win_hist ~ ACCOUNT_ID, data=total, sum, na.rm = T) 
total$win_hist <- NULL

#################################
# 0. Test feature complete ######
#################################
test$flag_regr <- 0
test$flag_class <- 'M'

##########################
# 1. New past hist #######
##########################
total <- merge(total, win_hist, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test <- merge(test, win_hist, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

#################################
# 1.5 Combine Total & Test ######
#################################
all <- rbind(total, test); str(all)
all$COUNTRY_OF_RESIDENCE_NAME <- NULL

########################
# 2. Imputation 1 ######
########################
all$AVG_PLACED_TAKEN_TIME_INPLAY[is.na(all$AVG_PLACED_TAKEN_TIME_INPLAY)] <- median(all$AVG_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all$AVG_PLACED_TAKEN_TIME_OUTPLAY[is.na(all$AVG_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all$AVG_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all$STDEV_PLACED_TAKEN_TIME_INPLAY[is.na(all$STDEV_PLACED_TAKEN_TIME_INPLAY)] <- median(all$STDEV_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all$STDEV_PLACED_TAKEN_TIME_OUTPLAY[is.na(all$STDEV_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all$STDEV_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all$SKEW_PLACED_TAKEN_TIME_INPLAY[is.na(all$SKEW_PLACED_TAKEN_TIME_INPLAY)] <- median(all$SKEW_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all$SKEW_PLACED_TAKEN_TIME_OUTPLAY[is.na(all$SKEW_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all$SKEW_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all$KURT_PLACED_TAKEN_TIME_INPLAY[is.na(all$KURT_PLACED_TAKEN_TIME_INPLAY)] <- median(all$KURT_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all$KURT_PLACED_TAKEN_TIME_OUTPLAY[is.na(all$KURT_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all$KURT_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all$STDEV_TAKEN_HOUR_INPLAY[is.na(all$STDEV_TAKEN_HOUR_INPLAY)] <- median(all$STDEV_TAKEN_HOUR_INPLAY, na.rm=T)
all$STDEV_TAKEN_HOUR_OUTPLAY[is.na(all$STDEV_TAKEN_HOUR_OUTPLAY)] <- median(all$STDEV_TAKEN_HOUR_OUTPLAY, na.rm=T)
all$PREV_WIN_RATE_INPLAY[is.na(all$PREV_WIN_RATE_INPLAY)] <- median(all$PREV_WIN_RATE_INPLAY, na.rm=T)
all$PREV_WIN_RATE_OUTPLAY[is.na(all$PREV_WIN_RATE_OUTPLAY)] <- median(all$PREV_WIN_RATE_OUTPLAY, na.rm=T)
all$PREV_WIN_RATE[is.na(all$PREV_WIN_RATE)] <- median(all$PREV_WIN_RATE, na.rm=T)
all$NET_PROFIT_INPLAY[is.na(all$NET_PROFIT_INPLAY)] <- median(all$NET_PROFIT_INPLAY, na.rm=T)
all$NET_PROFIT_OUTPLAY[is.na(all$NET_PROFIT_OUTPLAY)] <- median(all$NET_PROFIT_OUTPLAY, na.rm=T)
all$MARGIN_INPLAY[is.na(all$MARGIN_INPLAY)] <- median(all$MARGIN_INPLAY, na.rm=T)
all$MARGIN_OUTPLAY[is.na(all$MARGIN_OUTPLAY)] <- median(all$MARGIN_OUTPLAY, na.rm=T)
all$BL_RATIO_INPLAY[is.na(all$BL_RATIO_INPLAY)] <- median(all$BL_RATIO_INPLAY, na.rm=T)
all$BL_RATIO_OUTPLAY[is.na(all$BL_RATIO_OUTPLAY)] <- median(all$BL_RATIO_OUTPLAY, na.rm=T)
all$BL_RATIO[is.na(all$BL_RATIO)] <- median(all$BL_RATIO, na.rm=T)
all$win_hist[is.na(all$win_hist)] <- 0
all$AVG_TAKEN_HOUR_INPLAY[is.na(all$AVG_TAKEN_HOUR_INPLAY)] <- median(all$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
all$AVG_TAKEN_HOUR_OUTPLAY[is.na(all$AVG_TAKEN_HOUR_OUTPLAY)] <- median(all$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)

apply(all,2, function(x) mean(is.na(x)))

##########################
# 3. Invest feature ######
##########################
all$INVEST <- all$TRANSACTION_COUNT_INPLAY * all$AVG_BET_SIZE_INPLAY + all$TRANSACTION_COUNT_OUTPLAY * all$AVG_BET_SIZE_OUTPLAY

##############################
# 4. Log transformation ######
##############################
log_names <- names(all)[c(3:56, 59:60)]
for (col in log_names){
    all[,col] <- log_trans(all[,col])
}

##########################
# 5. Kmeans Cluster ######
##########################

##########################
# 6. GBDT Meta Data ######
##########################

######################
# 7. Validation ######
######################
# c(101183757,101183885,101184013) - last 3 event
# c(101150834,101153072,101149398)
# c(101093076,101093194,101093312) 
# c(101128387,101150348,101152275) 
# c(101149870,101150716,101153308)
all <- all[,c(1:56, 59:60, 58, 57)]

test <- all[all$flag_class == 'M', ]
total <- all[all$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101183757,101183885,101184013),]
train <- total[!total$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(train); dim(validation)

###################
# 8. Output #######
###################
save(train, validation, total, test, file='data/9_train_validation_test_log.RData')
