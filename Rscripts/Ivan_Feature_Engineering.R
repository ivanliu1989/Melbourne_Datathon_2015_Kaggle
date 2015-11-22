setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); library(caret)
load('data/Ivan_train_test_20151116.RData');ls()

#################################
# 1. Combine Total & Test #######
#################################
train$flag_class <- ifelse(train$flag_regr > 0 , 'Y', 'N')
test$flag_class <- 'M'

all_n <- rbind(train[train$flag_regr!=0,], test_new);dim(all_n)
all <- rbind(train[train$flag_regr!=0,], test);dim(all)
apply(all,2, function(x) mean(is.na(x)))
all_n <- all_n[,!apply(all_n,2, function(x) mean(is.na(x))) > 0.01726905]
all_n[is.na(all_n)] <- 0
apply(all_n,2, function(x) mean(is.na(x)))

###########################
# 2. tsne dimensions ######
###########################
feat <- c(3:(ncol(all)-2))
library(readr); library(Rtsne); library(ggplot2)
tsne <- Rtsne(as.matrix(all[,feat]), check_duplicates = FALSE, pca = TRUE, 
              perplexity=30, theta=0.5, dims=2)

embedding <- as.data.frame(tsne$Y)
embedding$Class <- as.factor(sub("Class_", "", all$flag_class)) # 27, 60

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
tsne_2d <- embedding[,1:2]; names(tsne_2d) <- c('tsne_3d_1','tsne_3d_2')

all <- cbind(all, tsne_2d)
all_n <- cbind(all_n, tsne_2d)

all <- all[,c(1:43, 46:47, 44:45)]
#################
# 3. PCA ########
#################
feat <- c(3:(ncol(all)-2))
library(caret)
INVEST <- all$TOTAL_BET_SIZE
prePro <- preProcess(all[,feat], method = c("center", "scale"))
all[,feat] <- predict(prePro, all[,feat])
all <- cbind(all, INVEST=INVEST)

# prePro <- preProcess(all$flag_regr, method = 'scale')
# prePro <- preProcess(all[,feat], method = c("pca"), thresh = 0.99)
# all_pca <- predict(prePro, all[,feat])
# all_pca <- cbind(all[,1:2], all_pca, all[, 44:45], INVEST=INVEST)
# all <- all_pca

######################
# 4. Validation ######
######################
# c(101183757,101183885,101184013) - last 3 event
# c(101150834,101153072,101149398)
# c(101093076,101093194,101093312) 
# c(101128387,101150348,101152275) 
# c(101149870,101150716,101153308)
test <- all[all$flag_class == 'M', ]
total <- all[all$flag_class != 'M', ]

#############################
# 5. fm meta feature ########
#############################
ffm_feat <- read.csv('ffm_meta_feature.csv', header = F)
ffm_feat_test <- read.csv('ffm_meta_feature_test.csv', header = F)
test <- cbind(test, ffm_meta = ffm_feat_test[,1])
total <- cbind(total, ffm_meta = ffm_feat[,1])
test <- test[,c(1:45, 49, 46:48)]
total <- total[,c(1:45, 49, 46:48)]

validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
dim(train); dim(validation)
save(train, validation, total, test, file='../Ivan_Train_Test_Scale_Center_20151121.RData')

# test
# all_n <- all_n[,c(1:25, 28:30, 26,27)]
# test <- all_n[all_n$flag_class == 'M', ]
# total <- all_n[all_n$flag_class != 'M', ]
# validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
# train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
# dim(train); dim(validation)
# 
# save(train, validation, total, test, file='data/S9_train_validation_test_20151110_test.RData')
