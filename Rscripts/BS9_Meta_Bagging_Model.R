setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
load('../S9_train_validation_test_20151110.RData');ls()
# load('data/S9_train_validation_test_20151110_test.RData');ls()
options(scipen=999);set.seed(19890624)
# localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

train <- train
test <- validation
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(3:(ncol(train)-2))] # train
# feat <- colnames(train)[c(3:(ncol(train)-3), ncol(train))] # test

#############################
### Raw prediction ##########
#############################
dtrain <- xgb.DMatrix(as.matrix(train[,feat]), label = train$flag_class)
dtest <- xgb.DMatrix(as.matrix(test[,feat]), label = test$flag_class)
# watchlist <- list(eval = dtest, train = dtrain)

# # Train the model
    bst <-
        xgb.train(
            data = dtrain, max.depth = 6, eta = 0.25, nround = 250, maximize = F, min_child_weight = 2, colsample_bytree = 0.8,
            nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1, gamma = 0.1
            #,watchlist = watchlist
            )

# # Make prediction
testPredictions_xb = predict(bst,dtest)
# testPredictions_nn = predict(bst,dtest)

#################################
### Meta bagging Model ##########
#################################
# testPredictions <- matrix(0, nrow = nrow(test), ncol = 1)
bootRounds = 1:20

for (j in bootRounds) {
  print(j)
  # Do the bootstrap resampling
  baggedIndex = sample(nrow(train), size = nrow(train), replace = T)
  OOBIndex = setdiff(1:nrow(train), baggedIndex)
  
  # 1. Build the OOB model ############
    # randomforest
  OOBModel = randomForest(x=train[OOBIndex,feat], y=as.factor(train$flag_class[OOBIndex]), replace=F, ntree=200, do.trace=T, mtry=7)
  bagPredictions_rf = predict(OOBModel, train[baggedIndex,], type="prob")
  bagTestPredictions_rf = predict(OOBModel, test, type="prob")
  
    # sofia svm
  train_svm <- train[baggedIndex, feat];
  train_svm_t <- train[OOBIndex, feat]; test_svm <- test[,feat]
  prep <- preProcess(train_svm, method = c('center',"scale"), verbose =T)
  train_svm <- cbind(predict(prep, train_svm),flag_class=train$flag_class[baggedIndex])
  train_svm_t <- cbind(predict(prep, train_svm_t),flag_class=train$flag_class[OOBIndex])
  test_svm <- cbind(predict(prep, test_svm),flag_class=test$flag_class)
  fit <- sofia(flag_class ~ ., data=train_svm_t, lambda = 1e-3, iiterations = 1e+10, random_seed = 13560,
               learner_type = 'logreg-pegasos', eta_type = 'pegasos', loop_type = 'balanced-stochastic', 
               rank_step_probability = 0.5, passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 78,
               perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0
  )
  bagPredictions_svm <- predict(fit, newdata=train_svm, prediction_type = "logistic")
  bagTestPredictions_svm <- predict(fit, newdata=test_svm, prediction_type = "logistic")
  
    # xg glm
  dtrain <- xgb.DMatrix(as.matrix(train[OOBIndex,feat]), label = train$flag_class[OOBIndex])
  dtrain_t <- xgb.DMatrix(as.matrix(train[baggedIndex,feat]), label = train$flag_class[baggedIndex])
  dtest <- xgb.DMatrix(as.matrix(test[,feat]), label = test$flag_class)
  bst <- xgb.train(
      data = dtrain, nround = 500, objective = "binary:logistic", booster = "gblinear", eta = 0.15,
      nthread = 4, alpha = 1e-3, lambda = 1e-6, verbose = 0
  )
  bagPredictions_glm = predict(bst, dtrain_t)
  bagTestPredictions_glm = predict(bst, dtest)
  
    # deep learning
  test_df <- as.h2o(localH2O, cbind(test[,feat], flag_class=test$flag_class))
  train_df <- as.h2o(localH2O, cbind(train[OOBIndex,feat],flag_class=train$flag_class[OOBIndex]))
  train_df_t <- as.h2o(localH2O, cbind(train[baggedIndex,feat],flag_class=train$flag_class[baggedIndex]))
  BagModel <-
      h2o.deeplearning(
          y = 'flag_class', x = names(train_df), data = train_df, classification = T,
          activation = "RectifierWithDropout",
          hidden = c(128,64), adaptive_rate = T, rho = 0.99, 
          epsilon = 1e-4, rate = 0.15, rate_decay = 0.9, # rate_annealing = , 
          momentum_start = 0.5, momentum_stable = 0.99, # momentum_ramp
          nesterov_accelerated_gradient = T, input_dropout_ratio = 0, hidden_dropout_ratios = c(0.5,0.5), 
          l2 = 3e-6, max_w2 = 4, 
          loss = 'CrossEntropy', classification_stop = -1,
          diagnostics = T, variable_importances = F, ignore_const_cols = T,
          force_load_balance = T, sparse = F, epochs = 90 
      )
  bagPredictions_nn = as.data.frame(h2o.predict(object = BagModel, newdata = train_df_t))
  bagTestPredictions_nn = as.data.frame(h2o.predict(object = BagModel, newdata = test_df))
  
  # 2. Build the Bag model ############
     # xgboost
  dtrain <- xgb.DMatrix(as.matrix(cbind(train[baggedIndex,feat],rf_n=bagPredictions_rf[,1],rf_y=bagPredictions_rf[,2],
                                        svm=bagPredictions_svm,glm=bagPredictions_glm,
                                        nn_n = bagPredictions_nn$X0, nn_y=bagPredictions_nn$X1)), label = train$flag_class[baggedIndex])
  dtest <- xgb.DMatrix(as.matrix(cbind(test[,feat],rf_n=bagTestPredictions_rf[,1],rf_y=bagTestPredictions_rf[,2],
                                       svm=bagTestPredictions_svm,glm=bagTestPredictions_glm,
                                       nn_n = bagTestPredictions_nn$X0, nn_y=bagTestPredictions_nn$X1)),label = test$flag_class)
  # watchlist <- list(eval = dtest, train = dtrain)
  BagModel <- xgb.train(data = dtrain, max.depth = 6, eta = 0.15, nround = 500, #watchlist = watchlist, 
                        colsample_bytree = 0.8, min_child_weight = 4, verbose = 0, 
                        nthread = 4, objective = "binary:logistic"#, metrics = 'auc', gamma = 0.1
  )
  # Make test predictions and reshape them
  tmpPredictions = predict(BagModel,dtest)
  testPredictions_xb = testPredictions_xb + tmpPredictions
  
}
testPredictions_xb = testPredictions_xb/(j+1)
# testPredictions_nn = testPredictions_nn/(j+1)

#########################
### Validation ##########
#########################
val <- test
val$Y <- testPredictions_xb
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

val$SINGLE_INVEST <- val$INVEST * ifelse(val$Y>=0.5, 1,-1)
pred_fin3 <- aggregate(SINGLE_INVEST ~ ACCOUNT_ID, data=val, sum, na.rm=F)
pred_fin3$Y <- ifelse(pred_fin3$SINGLE_INVEST >=0, 1,0)
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin3$Y);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
