setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); library(caret)
load('data/S_complete_data_clean_up.RData');
load('data/S_complete_data_clean_up_test.RData');ls()

#################################
# 0. Test feature complete ######
#################################
test$flag_regr <- 0
test$flag_class <- 'M'

##########################
# 1. New past hist #######
##########################

event_count <- aggregate(EVENT_ID ~ ACCOUNT_ID, data=total, length); names(event_count) <- c('ACCOUNT_ID', 'EVENT_COUNT') 

total <- merge(total, event_count, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
test <- merge(test, event_count, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

load('data/NEW_FEATURE.RData');ls()
total <- merge(total, NEW_FEATURE_train, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID','EVENT_ID'))
test <- merge(test, NEW_FEATURE_test[,-2], all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))

total$NET_PROFIT_IN_PER_EVENT <- total$NET_PROFIT_INPLAY / total$EVENT_COUNT
total$NET_PROFIT_OUT_PER_EVENT <- total$NET_PROFIT_INPLAY / total$EVENT_COUNT
total$NET_PROFIT_ALL_PER_EVENT <- total$NET_PROFIT_INPLAY / total$EVENT_COUNT

test$NET_PROFIT_IN_PER_EVENT <- test$NET_PROFIT_INPLAY / test$EVENT_COUNT
test$NET_PROFIT_OUT_PER_EVENT <- test$NET_PROFIT_INPLAY / test$EVENT_COUNT
test$NET_PROFIT_ALL_PER_EVENT <- test$NET_PROFIT_INPLAY / test$EVENT_COUNT

#################################
# 1.5 Combine Total & Test ######
#################################
all_n <- rbind(total, test[is.na(test$EVENT_COUNT),])
dim(all_n)
all <- rbind(total, test[!is.na(test$EVENT_COUNT),])
dim(all)

apply(all,2, function(x) mean(is.na(x)))

##########################
# 3. Invest feature ######
##########################
all$INVEST <- all$TRANSACTION_COUNT_INPLAY * all$AVG_BET_SIZE_INPLAY + all$TRANSACTION_COUNT_OUTPLAY * all$AVG_BET_SIZE_OUTPLAY
all_n$INVEST <- all_n$TRANSACTION_COUNT_INPLAY * all_n$AVG_BET_SIZE_INPLAY + all_n$TRANSACTION_COUNT_OUTPLAY * all_n$AVG_BET_SIZE_OUTPLAY

all_n <- all_n[,c(1:15,49:60)]
apply(all_n,2, function(x) mean(is.na(x)))

###########################
# 4. tsne dimensions ######
###########################
# feat <- c(3:58,61:75)
feat_n <- c(3:76)

library(readr); library(Rtsne); library(ggplot2)
tsne <- Rtsne(as.matrix(all[,feat]), check_duplicates = FALSE, pca = TRUE, 
              perplexity=30, theta=0.5, dims=2)

# tsne <- Rtsne(as.matrix(all_n[,feat_n]), check_duplicates = FALSE, pca = TRUE, 
#               perplexity=30, theta=0.5, dims=3)

embedding <- as.data.frame(tsne$Y)
embedding$Class <- as.factor(sub("Class_", "", all[,28])) # 27, 60

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
tsne_3d_test <- embedding[,1:3]; names(tsne_3d_test) <- c('tsne_3d_1', 'tsne_3d_2', 'tsne_3d_3')
tsne_2d <- embedding[,1:2]; names(tsne_2d) <- c('tsne_3d_1', 'tsne_3d_2')

# load('tsne_dimemsions.RData')
save(tsne_3d, tsne_3d_test, file='S_tsne_dimemsions_new.RData')

all <- cbind(all, tsne_2d)
all_n <- cbind(all_n, tsne_3d_test)

##########################
# 5. Kmeans Cluster ######
##########################
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

######################################
# Class Distance Calculations ########
######################################
library(caret)
feat <- colnames(all)[c(3:76)]
dt <- train[,feat]
centroids <- classDist(as.matrix(dt), as.factor(train$flag_class))
distances <- predict(centroids, dt)
distances <- as.data.frame(distances)
head(distances)

# xyplot(dist.Y ~ dist.N,
#        data = distances,
#        groups = as.factor(all$flag_class),
#        auto.key = list(columns = 2))

all <- cbind(all, distances[,c(2,3)])

######################
# 7. Validation ######
######################
# c(101183757,101183885,101184013) - last 3 event
# c(101150834,101153072,101149398)
# c(101093076,101093194,101093312) 
# c(101128387,101150348,101152275) 
# c(101149870,101150716,101153308)
all <- all[,c(1:58,61:78,59:60)]

test <- all[all$flag_class == 'M', ]
total <- all[all$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
dim(train); dim(validation)

save(train, validation, total, test, file='data/S9_train_validation_test_20151110.RData')

# test
all_n <- all_n[,c(1:25, 28:30, 26,27)]
test <- all_n[all_n$flag_class == 'M', ]
total <- all_n[all_n$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
dim(train); dim(validation)

save(train, validation, total, test, file='data/S9_train_validation_test_20151110_test.RData')
