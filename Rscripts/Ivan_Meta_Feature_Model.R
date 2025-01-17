setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(caret);
# load('data/9_train_validation_test_20151202.RData');ls()
load('data/9_train_validation_test_20151122.RData');
options(scipen=999);set.seed(19890624)

########################################
### Meta model random forest ###########
########################################
# OOBModel = randomForest(x=train[,feat], y=as.factor(train$flag_class), replace=F, ntree=100, do.trace=T, mtry=7)
# bagPredictions_rf = predict(OOBModel, train, type="prob")
# bagTestPredictions_rf = predict(OOBModel, test, type="prob")
# bagValidPredictions_rf = predict(OOBModel, validation, type="prob")

### Create Noise
# Nosify <- function(dt, noise_l = -0.00001, noise_u = 0.00001) {
#     dt_noise <- dt
#     dt_noise[,3:(ncol(dt)-2)]<-dt_noise[,3:(ncol(dt)-2)] + matrix(runif(prod(dim(dt[,3:(ncol(dt)-2)])), noise_l, noise_u), dim(dt[,3:(ncol(dt)-2)])[1])
#     head(dt_noise)
#     dt_noise <- rbind(dt_noise, dt); dim(dt_noise)
#     dt_noise <- dt_noise[sample(1:nrow(dt_noise),size = nrow(dt_noise)),]; dim(dt_noise)
#     return(dt_noise)
# }

Nosify <- function(dt, noise_l = -0.00001, noise_u = 0.00001, feat = c(7:41,57)) {
    dt_noise <- dt
    dt_noise[,feat]<-dt_noise[,feat] + 
        apply(dt[,feat], 2, 
              function(x){
                  runif(length(x), noise_l*diff(range(x)), noise_u*diff(range(x)))
                  })
    dt_noise <- rbind(dt_noise, dt); dim(dt_noise)
    dt_noise <- dt_noise[sample(1:nrow(dt_noise),size = nrow(dt_noise)),]
    return(dt_noise)
}


#########################
### Xgboost #############
#########################
# train$flag_margin <- train$flag_regr/train$INVEST
# test$flag_margin <- test$flag_regr/test$INVEST
# validation$flag_margin <- validation$flag_regr/validation$INVEST
# reg:linear
# binary:logistic

# 1. GBM
for (i in 1:20){
    set.seed(8*i)
    
    inTraining <- createDataPartition(total$flag_class, p = .3, list = FALSE)
    test <- test#total[train$EVENT_ID %in% c(101183757,101183885,101184013),]
    train <- total#[-inTraining,]#total[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
    train_noise <- Nosify(train,0,0.0003,c(3:57))
    # train_noise <- Nosify(train,0,0.0001)
    validation <- total[inTraining,]#total[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
    train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
    test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
    train_noise$flag_class <- ifelse(train_noise$flag_class == 'Y', 1, 0)
    validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, 0)
    feat <- colnames(train)[c(3:(ncol(train)-2))] # train
    
    dtrain <- xgb.DMatrix(as.matrix(train[,feat]), label = train$flag_class)
    dtest <- xgb.DMatrix(as.matrix(test[,feat]),label = test$flag_class)
    dvalid <- xgb.DMatrix(as.matrix(validation[,feat]),label = validation$flag_class)
    dtrain_noise <- xgb.DMatrix(as.matrix(train_noise[,feat]), label = train_noise$flag_class)
    watchlist <- list(eval = dvalid, train = dtrain_noise)
    
    bst <-
        xgb.train( # max.depth = 6, eta = 0.02, nround = 1200,
            data = dtrain_noise, max.depth = 6, eta = 0.02, nround = 1200, maximize = F, min_child_weight = 1, colsample_bytree = 1, #early.stop.round = 100,
            nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', #num_parallel_tree = 1, gamma = 0.1,
            watchlist = watchlist
        )
    p_gbm = predict(bst,dtest)
    # p_gbm = predict(bst,dvalid)
    write.csv(p_gbm, paste0('ReadyForBlending/submission/test/xgboost_gbm/submission_xgboost_gbm_20151208_noise_1pct',i,'.csv'))
    # write.csv(p_gbm, paste0('submission_xgboost_20151206.csv'))
    
    # 3. generalized linear model
    bst <- xgb.train(
        data = dtrain, nround = 550, watchlist = watchlist, objective = "binary:logistic", booster = "gblinear", eta = 0.3,
        nthread = 4, alpha = 1e-3, lambda = 1e-6, print.every.n = 10
    )
    p_glm = predict(bst,dtest)
    # p_glm = predict(bst,dvalid)
    write.csv(p_glm, paste0('ReadyForBlending/submission/test/xgboost_glm/submission_xgboost_glm_20151206_noise',i,'.csv'))
}

# # 2. RF
# bst <-
#     xgb.train(
#         data = dtrain, max.depth = 9, eta = 0.15, nround = 1, maximize = F, watchlist = watchlist, min_child_weight = 2, colsample_bytree = 0.8,
#         nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1000, gamma = 0.1
#     )
# # p_rf = predict(bst,dtest)
# p_rf = predict(bst,dvalid)
# 

p <- 0.75*p_gbm + 0.25*p_glm

#########################
### Validation ##########
#########################
# val <- test
val <- validation
# p <- ifelse(p_gbm>0.5, 1, 0)
val$Y <- p_gbm
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, sum, na.rm=F)
pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

### Model Performance ###
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin[,2]);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin2[,2]);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

prediction <- as.factor(ifelse(pred_fin[,2] >=0.5, 1, 0))
confusionMatrix(as.factor(val_fin$PRED_PROFIT_LOSS_3), prediction)

### New Calc
rocobj <- roc(val$flag_class, val$Y);print(auc(rocobj)) 
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

# 0.281524
# 0.8297 0.7236
# 0.8202 0.7145
# 0.7337
# 0.8006 0.7025


### Plot & feature importance ###
# Get the feature real names
names <- dimnames(as.matrix(train[,feat]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)
# Nice graph
xgb.plot.importance(importance_matrix)
