setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('../S9_train_validation_test_20151110.RData');ls()
# load('data/S9_train_validation_test_20151110_test.RData')
# load('data/9_train_validation_test_20151108.RData')

### Test
# train <- total
### Validation
training <- train[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
testing <- train[train$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(training); dim(testing)
training$flag_class <- ifelse(training$flag_class == 'Y', 1, 0)
testing$flag_class <- ifelse(testing$flag_class == 'Y', 1, 0)
validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, 0)
feat <- colnames(training)[c(3:(ncol(training)-2))]
# feat <- colnames(training)[c(3:57)]

################
### Training ###
################
    dtrain <- xgb.DMatrix(as.matrix(training[,feat]), label = training$flag_class)
    dtest <- xgb.DMatrix(as.matrix(validation[,feat]), label = validation$flag_class)
    watchlist <- list(eval = dtest, train = dtrain)

### tree
#     bst <-
#         xgb.train(
#             data = dtrain, max.depth = 6, eta = 0.15, nround = 500, maximize = F, watchlist = watchlist, min_child_weight = 4, colsample_bytree = 0.8,
#             nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1, gamma = 0.1
#         )

    bst <-
        xgb.train(
            data = dtrain, max.depth = 9, eta = 0.15, nround = 1, maximize = F, watchlist = watchlist, min_child_weight = 4, colsample_bytree = 0.8,
            nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1000, gamma = 0.1
        )
    
### generalized linear model
#     bst <- xgb.train(
#         data = dtrain, nround = 1500, watchlist = watchlist, objective = "binary:logistic", booster = "gblinear", eta = 0.1,
#         nthread = 4, alpha = 1e-3, lambda = 1e-6, print.every.n = 10
#     )


### Predict
    val <- validation
    # val <- testing
    p <- predict(bst, dtest)
    val$Y <- p
    tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
    val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y#(val$Y - 0.5) * 2
    pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)

### Validation
    val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
    val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

### Model Performance ###
    v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);print(auc(rocobj)) # Invest * Possibility
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);print(auc(rocobj)) # Average Possibility
    print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
    
    prediction <- as.factor(ifelse(v$INVEST_PERCENT >=0.5, 1, 0))
    confusionMatrix(as.factor(v$PRED_PROFIT_LOSS_3), prediction)

### Plot & feature importance ###
    model <- xgb.dump(bst, with.stats = T)
    model[1:10]
        # Get the feature real names
    names <- dimnames(as.matrix(train[,feat]))[[2]]
        # Compute feature importance matrix
    importance_matrix <- xgb.importance(names, model = bst)
        # Nice graph
    xgb.plot.importance(importance_matrix)
    xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 1)