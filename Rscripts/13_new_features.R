setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); library(caret)
source('Rscripts/12_log_transformation.R')
load('data/1_complete_data_new.RData');
load('data/2_test_new.RData');ls()


#################################
# 0. Test feature complete ######
#################################
test$flag_regr <- 0
test$flag_class <- 'M'

##########################
# 1. New past hist #######
##########################
# total$win_hist <- ifelse(total$flag_regr > 0, 1, ifelse(total$flag_regr <0, -1, 0)) 
# win_hist <- aggregate(win_hist ~ ACCOUNT_ID, data=total, sum, na.rm = T) 
# event_count <- aggregate(EVENT_ID ~ ACCOUNT_ID, data=total, length); names(event_count) <- c('ACCOUNT_ID', 'EVENT_COUNT') 
# total$win_hist <- NULL

# total <- merge(total, win_hist, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
# test <- merge(test, win_hist, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
# 
# total <- merge(total, event_count, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
# test <- merge(test, event_count, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

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
# all$win_hist[is.na(all$win_hist)] <- 0
all$AVG_TAKEN_HOUR_INPLAY[is.na(all$AVG_TAKEN_HOUR_INPLAY)] <- median(all$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
all$AVG_TAKEN_HOUR_OUTPLAY[is.na(all$AVG_TAKEN_HOUR_OUTPLAY)] <- median(all$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)

# apply(all,2, function(x) mean(is.na(x)))

##########################
# 3. Invest feature ######
##########################
all$INVEST <- all$TRANSACTION_COUNT_INPLAY * all$AVG_BET_SIZE_INPLAY + all$TRANSACTION_COUNT_OUTPLAY * all$AVG_BET_SIZE_OUTPLAY

##############################
# 4. Log transformation ######
##############################
# log_names <- names(all)[c(3:56, 59:60)]
# for (col in log_names){
#     all[,col] <- log_trans(all[,col])
# }

##########################
# 5. Kmeans Cluster ######
##########################
# feat <- c(1:2,3:4,7:8,11:12,15:16,19:20,47:56,60)
# names(all[,feat])
# kmean_dt <- KmeansClusters(all, k = 6, nstart = 50, feat)
# table(kmean_dt$CLUSTER)

# h2o
# library(h2o)
# localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')
# kmeans_df <- as.h2o(localH2O, all[,feat])
# cols <- c(colnames(kmeans_df[,3:(ncol(kmeans_df))]))
# fit <- h2o.kmeans(kmeans_df, centers = 6, cols=cols, iter.max = 100000, normalize = T, init = 'none') #none, plusplus, furthest
# pred <- as.data.frame(h2o.predict(object = fit, newdata = kmeans_df))
# all$kmeans <- pred[,1]; table(all$kmeans)

######################################
# Class Distance Calculations ########
######################################
library(caret)
feat <- colnames(all)[c(3:22,47:56)]
dt <- all[,feat]
centroids <- classDist(dt, as.factor(all$flag_class))
distances <- predict(centroids, dt)
distances <- as.data.frame(distances)
head(distances)

# xyplot(dist.Y ~ dist.N,
#        data = distances,
#        groups = as.factor(all$flag_class),
#        auto.key = list(columns = 2))

all <- cbind(all, distances[,c(2,3)])

##########################
# 6. GBDT Meta Data ######
##########################
# library(xgboost);library(pROC);
# set.seed(18)
# dim(all)
# all$flag_class <- ifelse(all$flag_class == 'Y', 1, 0)
# feat <- c(3:56,59:60)
# bst <- xgboost( data = as.matrix(all[,feat]), label = all$flag_class, max.depth = 7, eta = 0.2, nround = 30, maximize = F,
#         nthread = 4, objective = "binary:logistic", verbose = 1, early.stop.round = 10, print.every.n = 10, metrics = 'auc')
# p <- predict(bst, as.matrix(all[,feat])) 
# table(p)
# all$GBDT <- p; table(all$GBDT)

######################
# 7. Validation ######
######################
# c(101183757,101183885,101184013) - last 3 event
# c(101150834,101153072,101149398)
# c(101093076,101093194,101093312) 
# c(101128387,101150348,101152275) 
# c(101149870,101150716,101153308)
all <- all[,c(1:56, 59:63, 58, 57)]

test <- all[all$flag_class == 'M', ]
total <- all[all$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
dim(train); dim(validation)

###################
# 8. Output #######
###################
save(train, validation, total, test, file='data/9_train_validation_test_20151105.RData')
