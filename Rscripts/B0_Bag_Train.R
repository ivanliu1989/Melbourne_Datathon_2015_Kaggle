setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
# load('../S9_train_validation_test_20151110.RData');ls()
# load('data/S9_train_validation_test_20151110_test.RData')
load('data/9_train_validation_test_20151108.RData')

### Test
# train <- total
### Validation
training <- train[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
testing <- train[train$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(training); dim(testing)
training$flag_class <- ifelse(training$flag_class == 'Y', 1, 0)
# feat <- colnames(training)[c(3:76)]
feat <- colnames(training)[c(3:57)]

param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc", #auc logloss error
              "num_class" = 2,
              "nthread" = 4)

bst.cv = xgb.cv(param=param, data = as.matrix(training[,feat]), label = training$flag_class, 
                nfold = 4, column_subsample = 1, stratified = T, #subsample changes only hurt.
                nrounds=500, max.depth=6, eta=0.2, min_child_weight=10)


################
### Training ###
################
bst <-
    xgboost( 
        data = as.matrix(training[,feat]), label = training$flag_class, max.depth = 6, eta = 0.15, nround = 500, maximize = F, #500,0.15
        nthread = 4, objective = "binary:logistic", verbose = 1, early.stop.round = 10, print.every.n = 10, metrics = 'auc'
    )

bst <-
    xgboost( 
        data = as.matrix(training[,feat]), label = training$flag_regr, max.depth = 6, eta = 0.15, nround = 500, maximize = F, #500,0.15
        nthread = 4, objective = "reg:linear", verbose = 1, early.stop.round = 10, print.every.n = 10, metrics = 'auc'
    )

val <- validation
# val <- testing
p <- predict(bst, as.matrix(val[,feat])) 
val$Y <- p

tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y#(val$Y - 0.5) * 2
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
pred_fin3 <- aggregate(Y ~ ACCOUNT_ID, data=val, sum, na.rm=F); pred_fin3$Y_B <- ifelse(pred_fin3$Y > 0, 1, 0)

### Validation
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

#########################
### Model Performance ###
#########################
v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

rocobj <- roc(v$PRED_PROFIT_LOSS_3, pred_fin3$Y_B);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

prediction <- as.factor(ifelse(v$INVEST_PERCENT >=0.5, 1, 0))
confusionMatrix(as.factor(v$PRED_PROFIT_LOSS_3), prediction)
confusionMatrix(as.factor(v$PRED_PROFIT_LOSS_3), pred_fin3$Y_B)

#################################
### Plot & feature importance ###
#################################
model <- xgb.dump(bst, with.stats = T)
model[1:10]
# Get the feature real names
names <- dimnames(as.matrix(train[,feat]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)
# Nice graph
xgb.plot.importance(importance_matrix)
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 1)