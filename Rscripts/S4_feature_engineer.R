setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); library(caret)
# source('Rscripts/12_log_transformation.R')
load('data/1_complete_data_new.RData');
load('data/2_test_new.RData');ls()

# new_list <- all[is.na(all$PREV_WIN_RATE), 'ACCOUNT_ID']
# save(new_list, file='data/9_new_customers_list.RData')
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

# total <- merge(total, event_count, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
# test <- merge(test, event_count, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

##########################
# 1. New features  #######
##########################
load('data/NEW_FEATURE.RData');ls()
total <- merge(total, NEW_FEATURE_train, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
test <- merge(test, NEW_FEATURE_test[,-2], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

#################################
# 1.5 Combine Total & Test ######
#################################
all_n <- rbind(total, test[is.na(test$AVG_PLACED_TAKEN_TIME_INPLAY),])
# all_n$COUNTRY_OF_RESIDENCE_NAME <- NULL

all <- rbind(total, test[!is.na(test$AVG_PLACED_TAKEN_TIME_INPLAY),])
# all$COUNTRY_OF_RESIDENCE_NAME <- NULL

########################
# 2. Imputation 1 ######
########################
all$SD_PLACED_TAKEN[is.na(all$SD_PLACED_TAKEN)] <- 0
all$AVG_PLACED_TAKEN[is.na(all$AVG_PLACED_TAKEN)] <- 0
all$CANCEL_RATIO_OUTPLAY[is.na(all$CANCEL_RATIO_OUTPLAY)] <- 0
all$INPLAY_RATIO[is.na(all$INPLAY_RATIO)] <- 0

apply(all,2, function(x) mean(is.na(x)))

##########################
# 3. Invest feature ######
##########################
all$INVEST <- all$TRANSACTION_COUNT_INPLAY * all$AVG_BET_SIZE_INPLAY + all$TRANSACTION_COUNT_OUTPLAY * all$AVG_BET_SIZE_OUTPLAY
all_n$INVEST <- all_n$TRANSACTION_COUNT_INPLAY * all_n$AVG_BET_SIZE_INPLAY + all_n$TRANSACTION_COUNT_OUTPLAY * all_n$AVG_BET_SIZE_OUTPLAY

null_list <- apply(all_n,2, function(x) mean(is.na(x)))
all_n <- all_n[,colnames(all_n) %in% names(null_list[null_list==0])]

###########################
# 4. tsne dimensions ######
###########################
feat <- c(3:64,67:74)
feat_n <- c(3:34,37)

library(readr); library(Rtsne); library(ggplot2)
tsne <- Rtsne(as.matrix(all[,feat]), check_duplicates = FALSE, pca = TRUE, 
              perplexity=30, theta=0.5, dims=2)

# tsne <- Rtsne(as.matrix(all_n[,feat_n]), check_duplicates = FALSE, pca = TRUE, 
#               perplexity=30, theta=0.5, dims=2)

embedding <- as.data.frame(tsne$Y)
embedding$Class <- as.factor(sub("Class_", "", all[,58])) # 36, 58

p <- ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
    geom_point(size=1.25) +
    guides(colour = guide_legend(override.aes = list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("t-SNE 2D Embedding of Betting Data") +
    theme_light(base_size=20) +
    theme(strip.background = element_blank(),
          strip.text.x     = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_blank(),
          axis.ticks       = element_blank(),
          axis.line        = element_blank(),
          panel.border     = element_blank())
p
# tsne_2d_new <- embedding[,1:2]; names(tsne_2d_new) <- c('tsne_2d_new_1', 'tsne_2d_new_2')
# tsne_3d_new <- embedding[,1:3]; names(tsne_3d_new) <- c('tsne_3d_new_1', 'tsne_3d_new_2', 'tsne_3d_new_3')
# tsne_2d <- embedding[,1:2]; names(tsne_2d) <- c('tsne_2d_1', 'tsne_2d_2')
# tsne_3d <- embedding[,1:3]; names(tsne_3d) <- c('tsne_3d_1', 'tsne_3d_2', 'tsne_3d_3')

# load('tsne_dimemsions.RData')
save(tsne_3d_new, tsne_2d_new, tsne_3d,tsne_2d, file='tsne_dimemsions_new.RData')

all <- cbind(all, tsne_2d_sim, tsne_2d_comp, tsne_3d_sim, tsne_3d_comp)

##########################
# 5. Kmeans Cluster ######
##########################
# feat <- c(3:22,47:56,61)
# names(all[,feat])
# # kmean_dt <- KmeansClusters(all, k = 6, nstart = 50, feat)
# # table(kmean_dt$CLUSTER)
# 
# #h2o
# library(h2o)
# localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')
# kmeans_df <- as.h2o(localH2O, all[,feat])
# cols <- c(colnames(kmeans_df))
# fit <- h2o.kmeans(kmeans_df, centers = 6, cols=cols, iter.max = 100000, normalize = T, init = 'none') #none, plusplus, furthest
# pred <- as.data.frame(h2o.predict(object = fit, newdata = kmeans_df))
# all$kmeans_trans <- pred[,1]; table(all$kmeans_trans)
# 
# # kmeans_tsne_2d_sim
# kmeans_df <- as.h2o(localH2O, all[,62:63]) #62:63, 64:65, 66:68, 69:71
# cols <- c(colnames(kmeans_df))
# fit <- h2o.kmeans(kmeans_df, centers = 6, cols=cols, iter.max = 100000, normalize = T, init = 'none') #none, plusplus, furthest
# pred <- as.data.frame(h2o.predict(object = fit, newdata = kmeans_df))
# all$kmeans_tsne_2d_sim <- pred[,1]; table(all$kmeans_tsne_2d_sim)
# 
# # kmeans_tsne_2d_comp
# kmeans_df <- as.h2o(localH2O, all[,64:65]) #62:63, 64:65, 66:68, 69:71
# cols <- c(colnames(kmeans_df))
# fit <- h2o.kmeans(kmeans_df, centers = 6, cols=cols, iter.max = 100000, normalize = T, init = 'none') #none, plusplus, furthest
# pred <- as.data.frame(h2o.predict(object = fit, newdata = kmeans_df))
# all$kmeans_tsne_2d_comp <- pred[,1]; table(all$kmeans_tsne_2d_comp)
# 
# # kmeans_tsne_3d_sim
# kmeans_df <- as.h2o(localH2O, all[,66:68]) #62:63, 64:65, 66:68, 69:71
# cols <- c(colnames(kmeans_df))
# fit <- h2o.kmeans(kmeans_df, centers = 6, cols=cols, iter.max = 100000, normalize = T, init = 'none') #none, plusplus, furthest
# pred <- as.data.frame(h2o.predict(object = fit, newdata = kmeans_df))
# all$kmeans_tsne_3d_sim <- pred[,1]; table(all$kmeans_tsne_3d_sim)
# 
# # kmeans_tsne_3d_comp
# kmeans_df <- as.h2o(localH2O, all[,69:71]) #62:63, 64:65, 66:68, 69:71
# cols <- c(colnames(kmeans_df))
# fit <- h2o.kmeans(kmeans_df, centers = 6, cols=cols, iter.max = 100000, normalize = T, init = 'none') #none, plusplus, furthest
# pred <- as.data.frame(h2o.predict(object = fit, newdata = kmeans_df))
# all$kmeans_tsne_3d_comp <- pred[,1]; table(all$kmeans_tsne_3d_comp)

######################################
# Class Distance Calculations ########
######################################
library(caret)
feat <- colnames(all)[c(3:22,47:56,61)]
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
all <- all[,c(1:56,59:78,58,57)]

test <- all[all$flag_class == 'M', ]
total <- all[all$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
dim(train); dim(validation)

###################
# 8. Output #######
###################
save(train, validation, total, test, file='data/9_train_validation_test_20151108_feat.RData')
