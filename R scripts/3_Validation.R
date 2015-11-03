setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); library(caret)

load('data/1_complete_data_new.RData');
load('data/2_test_new.RData');ls()

test$flag_regr <- 0
test$flag_class <- 'M'

### Dummy Variable
all <- rbind(total, test)
str(all)
all$COUNTRY_OF_RESIDENCE_NAME <- NULL
all$AVG_TAKEN_HOUR_INPLAY <- as.factor(all$AVG_TAKEN_HOUR_INPLAY)
all$AVG_TAKEN_HOUR_OUTPLAY <- as.factor(all$AVG_TAKEN_HOUR_OUTPLAY)
# all$COUNTRY_OF_RESIDENCE_NAME <- as.factor(all$COUNTRY_OF_RESIDENCE_NAME)

library(caret)
dummies <- dummyVars(flag_class ~ ., data = all[,c('AVG_TAKEN_HOUR_INPLAY', 'AVG_TAKEN_HOUR_OUTPLAY', 'flag_class')])
dum <- predict(dummies, newdata = all[,c('AVG_TAKEN_HOUR_INPLAY', 'AVG_TAKEN_HOUR_OUTPLAY', 'flag_class')])
apply(dum,2, function(x) mean(is.na(x)))
dum[is.na(dum)] <- 0
all_c <- cbind(all[,!names(all) %in% c('AVG_TAKEN_HOUR_INPLAY', 'AVG_TAKEN_HOUR_OUTPLAY', 'flag_regr','flag_class')], 
               dum, all[,c('flag_regr','flag_class')])
all <- all_c

### Imputation
all_c <- all # No dummy variable

all_c$AVG_PLACED_TAKEN_TIME_INPLAY[is.na(all_c$AVG_PLACED_TAKEN_TIME_INPLAY)] <- median(all_c$AVG_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all_c$AVG_PLACED_TAKEN_TIME_OUTPLAY[is.na(all_c$AVG_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all_c$AVG_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all_c$STDEV_PLACED_TAKEN_TIME_INPLAY[is.na(all_c$STDEV_PLACED_TAKEN_TIME_INPLAY)] <- median(all_c$STDEV_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all_c$STDEV_PLACED_TAKEN_TIME_OUTPLAY[is.na(all_c$STDEV_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all_c$STDEV_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all_c$SKEW_PLACED_TAKEN_TIME_INPLAY[is.na(all_c$SKEW_PLACED_TAKEN_TIME_INPLAY)] <- median(all_c$SKEW_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all_c$SKEW_PLACED_TAKEN_TIME_OUTPLAY[is.na(all_c$SKEW_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all_c$SKEW_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all_c$KURT_PLACED_TAKEN_TIME_INPLAY[is.na(all_c$KURT_PLACED_TAKEN_TIME_INPLAY)] <- median(all_c$KURT_PLACED_TAKEN_TIME_INPLAY, na.rm=T)
all_c$KURT_PLACED_TAKEN_TIME_OUTPLAY[is.na(all_c$KURT_PLACED_TAKEN_TIME_OUTPLAY)] <- median(all_c$KURT_PLACED_TAKEN_TIME_OUTPLAY, na.rm=T)
all_c$STDEV_TAKEN_HOUR_INPLAY[is.na(all_c$STDEV_TAKEN_HOUR_INPLAY)] <- median(all_c$STDEV_TAKEN_HOUR_INPLAY, na.rm=T)
all_c$STDEV_TAKEN_HOUR_OUTPLAY[is.na(all_c$STDEV_TAKEN_HOUR_OUTPLAY)] <- median(all_c$STDEV_TAKEN_HOUR_OUTPLAY, na.rm=T)
all_c$PREV_WIN_RATE_INPLAY[is.na(all_c$PREV_WIN_RATE_INPLAY)] <- median(all_c$PREV_WIN_RATE_INPLAY, na.rm=T)
all_c$PREV_WIN_RATE_OUTPLAY[is.na(all_c$PREV_WIN_RATE_OUTPLAY)] <- median(all_c$PREV_WIN_RATE_OUTPLAY, na.rm=T)
all_c$PREV_WIN_RATE[is.na(all_c$PREV_WIN_RATE)] <- median(all_c$PREV_WIN_RATE, na.rm=T)
all_c$NET_PROFIT_INPLAY[is.na(all_c$NET_PROFIT_INPLAY)] <- median(all_c$NET_PROFIT_INPLAY, na.rm=T)
all_c$NET_PROFIT_OUTPLAY[is.na(all_c$NET_PROFIT_OUTPLAY)] <- median(all_c$NET_PROFIT_OUTPLAY, na.rm=T)
all_c$MARGIN_INPLAY[is.na(all_c$MARGIN_INPLAY)] <- median(all_c$MARGIN_INPLAY, na.rm=T)
all_c$MARGIN_OUTPLAY[is.na(all_c$MARGIN_OUTPLAY)] <- median(all_c$MARGIN_OUTPLAY, na.rm=T)
all_c$BL_RATIO_INPLAY[is.na(all_c$BL_RATIO_INPLAY)] <- median(all_c$BL_RATIO_INPLAY, na.rm=T)
all_c$BL_RATIO_OUTPLAY[is.na(all_c$BL_RATIO_OUTPLAY)] <- median(all_c$BL_RATIO_OUTPLAY, na.rm=T)
all_c$BL_RATIO[is.na(all_c$BL_RATIO)] <- median(all_c$BL_RATIO, na.rm=T)

apply(all_c,2, function(x) mean(is.na(x)))

### Validation set
all_c$INVEST <- all_c$TRANSACTION_COUNT_INPLAY * all_c$AVG_BET_SIZE_INPLAY + all_c$TRANSACTION_COUNT_OUTPLAY * all_c$AVG_BET_SIZE_OUTPLAY

###  1.Normal
# test <- all_c[all_c$flag_class == 'M', ]
# total <- all_c[all_c$flag_class != 'M', ]
# validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398) ,] #c(101150834,101153072,101149398)      c(101183757,101183885,101184013) - last 3 event
# train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398) ,]
# dim(train); dim(validation)

###  2.Center Scale
# prep <- preProcess(all_c[, -c(1,2,161,162,163)], method = c('center',"scale"), verbose =T)
# all_c[, -c(1,2,161,162,163)] <- predict(prep, all_c[, -c(1,2,161,162,163)])
# 
# test <- all_c[all_c$flag_class == 'M', ]
# total <- all_c[all_c$flag_class != 'M', ]
# validation <- total[total$EVENT_ID %in% c(101183757,101183885,101184013),]
# train <- total[!total$EVENT_ID %in% c(101183757,101183885,101184013),]
# dim(train); dim(validation)

###  3.PCA
# prepca <- preProcess(all_c[, -c(1,2,161,162,163)], method = c('pca'), verbose =T, thresh = 0.9999)
# pcadt <- predict(prepca, all_c[, -c(1,2,161,162,163)])
# all_pca <- cbind(all_c[, c(1,2)], pcadt, all_c[,c(161,162,163)])
# 
# test <- all_pca[all_pca$flag_class == 'M', ]
# total <- all_pca[all_pca$flag_class != 'M', ]
# validation <- total[total$EVENT_ID %in% c(101183757,101183885,101184013),]
# train <- total[!total$EVENT_ID %in% c(101183757,101183885,101184013),]
# dim(train); dim(validation)

### 4. No Dummy 
# c(101150834,101153072,101149398)
# c(101183757,101183885,101184013) - last 3 event
# c(101093076,101093194,101093312) 
# c(101128387,101150348,101152275) 
# c(101149870,101150716,101153308)
# all_c$AVG_TAKEN_HOUR_INPLAY[is.na(all_c$AVG_TAKEN_HOUR_INPLAY)] <- median(all_c$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
# all_c$AVG_TAKEN_HOUR_OUTPLAY[is.na(all_c$AVG_TAKEN_HOUR_OUTPLAY)] <- median(all_c$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)
# all_c$COUNTRY_OF_RESIDENCE_NAME[is.na(all_c$COUNTRY_OF_RESIDENCE_NAME)] <- 'UAE'

prep <- preProcess(all_c[,-which(names(all_c) %in% c('ACCOUNT_ID','EVENT_ID','AVG_TAKEN_HOUR_INPLAY','AVG_TAKEN_HOUR_OUTPLAY', 
                                                     "flag_regr","flag_class","INVEST"))], method = c('center',"scale"), verbose =T)
all_c[,-which(names(all_c) %in% c('ACCOUNT_ID','EVENT_ID','AVG_TAKEN_HOUR_INPLAY','AVG_TAKEN_HOUR_OUTPLAY', 
                                  "flag_regr","flag_class","INVEST"))] <- predict(prep, all_c[,-which(names(all_c) %in% c('ACCOUNT_ID','EVENT_ID','AVG_TAKEN_HOUR_INPLAY','AVG_TAKEN_HOUR_OUTPLAY', 
                                                                                                                          "flag_regr","flag_class","INVEST"))])
test <- all_c[all_c$flag_class == 'M', ]
total <- all_c[all_c$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101149870,101150716,101153308),]
train <- total[!total$EVENT_ID %in% c(101149870,101150716,101153308),]
dim(train); dim(validation)


### Output
# save(train, validation, total, test, file='data/3_train_validation_test.RData')
# save(train, validation, total, test, file='data/4_train_validation_test_center_scale.RData')
# save(train, validation, total, test, file='data/5_train_validation_test_pca.RData')
# save(train, validation, total, test, file='data/9_train_validation_test_TREE_5.RData')
save(train, validation, total, test, file='data/9_train_validation_test_ONEHOT_5.RData')



# AVG_PLACED_TAKEN_TIME_INPLAY   AVG_PLACED_TAKEN_TIME_OUTPLAY 
# 0.0204344                       0.0204344 
# STDEV_PLACED_TAKEN_TIME_INPLAY STDEV_PLACED_TAKEN_TIME_OUTPLAY   SKEW_PLACED_TAKEN_TIME_INPLAY  SKEW_PLACED_TAKEN_TIME_OUTPLAY 
# 0.0204344                       0.0204344                       0.0204344                       0.0204344 
# KURT_PLACED_TAKEN_TIME_INPLAY  KURT_PLACED_TAKEN_TIME_OUTPLAY         STDEV_TAKEN_HOUR_INPLAY        STDEV_TAKEN_HOUR_OUTPLAY 
# 0.0204344                       0.0204344                       0.0204344                       0.0204344 
# PREV_WIN_RATE_INPLAY           PREV_WIN_RATE_OUTPLAY                   PREV_WIN_RATE               NET_PROFIT_INPLAY 
# 0.0204344                       0.0204344                       0.0204344                       0.0204344 
# NET_PROFIT_OUTPLAY                   MARGIN_INPLAY                  MARGIN_OUTPLAY            
# 0.0204344                       0.0204344                       0.0204344                     
# BL_RATIO_INPLAY                BL_RATIO_OUTPLAY                        BL_RATIO 
# 0.0204344                       0.0204344                       0.0204344