setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(caret);library(pROC)
load('data/train_validation_test.RData');ls()


######################
### Training Model ###
######################
# Config
train$flag_class <- as.factor(train$flag_class)

fitControl <- trainControl(method = "cv",
                           number = 2,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
Grid <-  expand.grid(mtry=6)
# Grid <-  expand.grid(nrounds = 100, max_depth = 8, eta = 0.05) # xgbTree
# Grid <-  expand.grid(sigma = 1, C = 0.1) # svmRaidal
# Grid <-  expand.grid(size = 80, decay = 0.1) # nnet
# Grid <-  expand.grid(fL=0.01, usekernel=F) # nb
# Grid <-  expand.grid(nIter=20) # LogitBoost
# Grid <-  expand.grid(n.trees = 180, interaction.depth = 6, shrinkage = 0.01) # gbm

# Training
set.seed(825)
fit <- train(flag_class ~ ., data=train[,-c(1,2,53)], # classification
             method = "rf",
             trControl = fitControl,
             tuneGrid = Grid,
             # preProcess = c('center', 'scale'),
             metric ='ROC',
             verbose = T)


# Config 2
fitControl2 <- trainControl(method = "adaptive_cv",
                            number = 5,
                            repeats = 2,
                            ## Estimate class probabilities
                            classProbs = TRUE,
                            ## Evaluate performance using 
                            ## the following function
                            summaryFunction = twoClassSummary,
                            ## Adaptive resampling information:
                            adaptive = list(min = 8,
                                            alpha = 0.05,
                                            method = "BT", #gls for Linear
                                            complete = TRUE))

set.seed(825)
svmFit2 <- train(flag_class ~ ., data=train[,-c(1,2,53)],
                 method = "rf", #svmRadial
                 trControl = fitControl2,
                 # preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")

# Variable Imp
fitImp <- varImp(fit, scale = T)
fitImp[1]


##################
### Validation ###
##################
### Predict
val <- validation[!validation$COUNTRY_OF_RESIDENCE_NAME %in% c('Macau','Qatar'),]
p <- predict(fit, newdata=val, type = 'prob')
val$INVEST <- val$TRANSACTION_COUNT_INPLAY * val$AVG_BET_SIZE_INPLAY + val$TRANSACTION_COUNT_OUTPLAY * val$AVG_BET_SIZE_OUTPLAY
val$Y <- p$Y
val$PRED_PROFIT_LOSS <- (val$Y - 0.5) * val$INVEST * 2
pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=val, sum, na.rm=F)
pred_fin$PRED_PROFIT_LOSS_2 <- ifelse(pred_fin$PRED_PROFIT_LOSS > 0, 1, ifelse(pred_fin$PRED_PROFIT_LOSS < 0, 0, 0.5))

### Validation
# validation$flag_class <- as.factor(validation$flag_class)
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))

p_rf <- p
p_lg <- p
p$Y <- (p_rf$Y + p_lg$Y)/2
#########################
### Model Performance ###
#########################
v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')

# With a roc object:
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$PRED_PROFIT_LOSS_2)
rocobj <- roc(val$flag_class, p$Y)
# Full AUC:
auc(rocobj) 
# rf 0.7225 
# lg 0.7517

# Partial AUC:
auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE) 
# rf 0.5933
# lg 0.6529

# Plot
plot(rocobj)


############
### test ###
############
p <- predict(fit, newdata=test, type = 'prob')
test$INVEST <- test$TRANSACTION_COUNT_INPLAY * test$AVG_BET_SIZE_INPLAY + test$TRANSACTION_COUNT_OUTPLAY * test$AVG_BET_SIZE_OUTPLAY
test$Y <- p$Y
test$PRED_PROFIT_LOSS <- (test$Y - 0.5) * test$INVEST * 2

pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=test, sum, na.rm=T)

### Submission
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submission_20151028_1.csv',quote = FALSE,row.names = FALSE)
save(fit, file='../rf.RData')
